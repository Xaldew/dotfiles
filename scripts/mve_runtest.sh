#!/usr/bin/env bash
## Simplifies using the mve_decode tool
## Copies source material and outputs to local directory to speed up testing

function mve()
{
    # Print help.
    if [ $# -lt 2 ]; then
	echo -e "\n\tusage: mve [codec] [input video] [decoder options]\n\n" \
            "\t--cmd      print the full codec command instead of running it\n" \
            "\t--gdb-emul run the emulator command in gdb\n" \
            "\t--gdb-fw   run the firmware command in gdb\n" \
            "\t--less     pipe output to less (not gdb compatible)\n" \
            "\t--mbinfo   give the codec the --mbinfo flag and output .mbinfo\n" \
            "\t--valgrind run the emulator in valgrind\n"
	return 1
    fi
    # Setup dir structure
    mkdir -p /work/$USER/tmp;
    output="test.yuv"
    # Save arguments, since the shift in the while loop will consume them
    codec="$1";
    input="$2";
    input_filename="$(basename $2)"
    mve_app="mve_decode"
    # Initialize flags
    print_command=false
    gdb_emul=false
    gdb_fw=false
    valgrind=false
    # Remove codec and input path arguments
    shift; shift
    # Create an empty array of arguments to be passed onto the emulator
    args=()

    # If codec is an encoder, use encoder binary instead
    if [[ $codec == *enc* ]]; then
	mve_app="mve_encode"
    fi

    # Handle arguments
    for argument; do
        # Look for --cmd argument, if found output intended commands instead.
	if [ "$argument" = "--cmd" ]; then
	    print_command=true;
	    input_filename=$input
        # Look for --gdb-emul argument, if found run the emulator in gdb.
	elif [ "$argument" = "--gdb-emul" ]; then
	    gdb_emul=true;
        # Look for --gdb-fw argument, if found run the fw in gdb
	elif [ "$argument" = "--gdb-fw" ]; then
	    gdb_fw=true;
        # Look for --mbinfo argument, if found append ".mbinfo" to output filename
	elif [ "$argument" = "--mbinfo" ]; then
	    output="$output.mbinfo";
	    args+=($argument);
        # Look for --valgrind", run the emulator in valgrind
	elif [ "$argument" = "--valgrind" ]; then
	    valgrind=true;
	else
	    args+=($argument);
	fi
    done

    # Build executable command (needs to be declared after the args array.)
    command="$WORKSPACE_DIR/mve6/test/system/out/test-linux32/$mve_app \
--fwbin $WORKSPACE_DIR/mve6/test/system/out/codec-$codec/codec.fwbin \
--name $codec \
-i $input_filename \
-o $output ${args[@]}"

    (
	cd /work/$USER/tmp;
	rsync -aq $input .;

	if $print_command; then
	    input_filename=input
	    echo "$command"
	elif $gdb_emul; then
	    echo -e "set logging on\nset logging overwrite on\nset logging \
file $output.less" > gdb.commands
	    chmod +x gdb.commands
	    eval "gdb -x gdb.commands --args $command"
	elif $gdb_fw; then
	    echo -e "# Debug using GDB?\ngdb-enable=yes\n# Port number for GDB \
communication.\ngdb-port=$(id -u)" > profile.conf
            # Backup environment variable
	    env_backup=$DESKTOP_STARTUP_ID
            # Disable focus stealing in window manager
	    export DESKTOP_STARTUP_ID=_TIME0
	    gnome-terminal --title="emul" -x sh -c \
		"$command --config profile.conf 2>&1 | tee $output.less" &
            # Re-enable focus stealing
	    export DESKTOP_STARTUP_ID=$env_backup
	    echo -e "target remote localhost:$(id -u)" > gdb.commands
	    chmod +x gdb.commands
            # Give the emulator some time to start
            # (Or it won't accept the gdb connection)
	    sleep 0.2
	    rasc-elf-gdb \
		"$WORKSPACE_DIR/mve6/test/system/out/codec-$codec/codec.elf" \
		-x gdb.commands
	elif $valgrind; then
	    valgrind --track-origins=yes $command
	else
	    echo -e "codec\t\"$codec\""
	    echo -e "input\t\"$input_filename\""
	    echo -e "output\t\"$output\""
	    echo -e "args\t\"${args[@]}\""
	    eval "$command 2>&1 | tee $output.less"
	fi
    )
}

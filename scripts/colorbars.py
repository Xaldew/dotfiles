#!/usr/bin/env python
# Display the terminal color bars along the regular true-color equivalent.

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors


def create_colormaps():
    """Create a plain color map."""
    # Generate truecolor colormaps.
    lred    = [(c, 0, 0) for c in np.linspace(0, 1, 256)]
    lgreen  = [(0, c, 0) for c in np.linspace(0, 1, 256)]
    lblue   = [(0, 0, c) for c in np.linspace(0, 1, 256)]
    lgray   = [(c, c, c) for c in np.linspace(0, 1, 256)]

    plt.register_cmap(cmap=colors.ListedColormap(lred,   name="tc_red"))
    plt.register_cmap(cmap=colors.ListedColormap(lgreen, name="tc_green"))
    plt.register_cmap(cmap=colors.ListedColormap(lblue,  name="tc_blue"))
    plt.register_cmap(cmap=colors.ListedColormap(lgray,  name="tc_gray"))

    # Generate 256-color terminal colormaps.
    term256_hex = ["00","5f","87","af","d7","ff"]
    term256_hex_gray = ["00", "08", "12", "1c", "26", "30", "3A", "44", "4e",
                        "58", "62", "6c", "76", "80", "8a", "94", "9e", "a8",
                        "b2", "bc", "c6", "d0", "da", "e4", "ee"]
    term256_r = ["#{}0000".format(h) for h in term256_hex]
    term256_g = ["#00{}00".format(h) for h in term256_hex]
    term256_b = ["#0000{}".format(h) for h in term256_hex]
    term256_gray = ["#{0:s}{0:s}{0:s}".format(h) for h in term256_hex_gray]

    plt.register_cmap(cmap=colors.ListedColormap(term256_r,    name="term256_red"))
    plt.register_cmap(cmap=colors.ListedColormap(term256_g,    name="term256_green"))
    plt.register_cmap(cmap=colors.ListedColormap(term256_b,    name="term256_blue"))
    plt.register_cmap(cmap=colors.ListedColormap(term256_gray, name="term256_gray"))

    # Generate 88-color terminal colormaps.
    term88_hex = ["00", "8b", "cd", "ff"]
    term88_hex_gray = ["00", "2e", "5c", "73", "8b", "a2", "b9", "d0", "e7"]
    term88_r = ["#{}0000".format(h) for h in term88_hex]
    term88_g = ["#00{}00".format(h) for h in term88_hex]
    term88_b = ["#0000{}".format(h) for h in term88_hex]
    term88_gray = ["#{0:s}{0:s}{0:s}".format(h) for h in term88_hex_gray]

    plt.register_cmap(cmap=colors.ListedColormap(term88_r,    name="term88_red"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_g,    name="term88_green"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_b,    name="term88_blue"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_gray, name="term88_gray"))

    return ["tc_red", "term256_red", "term88_red",
            "tc_green", "term256_green", "term88_green",
            "tc_blue", "term256_blue", "term88_blue",
            "tc_gray", "term256_gray", "term88_gray"]

cmaps = [('Terminal',     create_colormaps())]


nrows = max(len(cmap_list) for cmap_category, cmap_list in cmaps)
gradient = np.linspace(0, 1, 256)
gradient = np.vstack((gradient, gradient))

def plot_color_gradients(cmap_category, cmap_list):
    fig, axes = plt.subplots(nrows=nrows)
    fig.subplots_adjust(top=0.95, bottom=0.01, left=0.2, right=0.99)
    axes[0].set_title(cmap_category + ' colormaps', fontsize=14)

    for ax, name in zip(axes, cmap_list):
        ax.imshow(gradient, aspect='auto', cmap=plt.get_cmap(name))
        pos = list(ax.get_position().bounds)
        x_text = pos[0] - 0.01
        y_text = pos[1] + pos[3]/2.
        fig.text(x_text, y_text, name, va='center', ha='right', fontsize=10)

    # Turn off *all* ticks & spines, not just the ones with colormaps.
    for ax in axes:
        ax.set_axis_off()

for cmap_category, cmap_list in cmaps:
    plot_color_gradients(cmap_category, cmap_list)

plt.show()

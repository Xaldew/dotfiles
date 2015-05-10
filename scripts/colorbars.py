#!/usr/bin/env python
# Display the terminal color bars along the regular true-color equivalent.


"""
Reference for colormaps included with Matplotlib.

This reference example shows all colormaps included with Matplotlib. Note that
any colormap listed here can be reversed by appending "_r" (e.g., "pink_r").
These colormaps are divided into the following categories:

Sequential:
    These colormaps are approximately monochromatic colormaps varying smoothly
    between two color tones---usually from low saturation (e.g. white) to high
    saturation (e.g. a bright blue). Sequential colormaps are ideal for
    representing most scientific data since they show a clear progression from
    low-to-high values.

Diverging:
    These colormaps have a median value (usually light in color) and vary
    smoothly to two different color tones at high and low values. Diverging
    colormaps are ideal when your data has a median value that is significant
    (e.g.  0, such that positive and negative values are represented by
    different colors of the colormap).

Qualitative:
    These colormaps vary rapidly in color. Qualitative colormaps are useful for
    choosing a set of discrete colors. For example::

        color_list = plt.cm.Set3(np.linspace(0, 1, 12))

    gives a list of RGB colors that are good for plotting a series of lines on
    a dark background.

Miscellaneous:
    Colormaps that don't fit into the categories above.

"""
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

    # Generate 88-color terminal colormaps.
    term88_r = ["#000000", "#8b0000", "#cd0000", "#ff0000"]
    term88_g = ["#000000", "#008b00", "#00cd00", "#00ff00"]
    term88_b = ["#000000", "#00008b", "#0000cd", "#0000ff"]
    term88_gray = ["#{0:s}{0:s}{0:s}".format(h) for h in
                   ["2e", "5c", "73", "8b", "a2", "b9", "d0", "e7"]]

    plt.register_cmap(cmap=colors.ListedColormap(term88_r,    name="term88_red"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_g,    name="term88_green"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_b,    name="term88_blue"))
    plt.register_cmap(cmap=colors.ListedColormap(term88_gray, name="term88_gray"))

    return ["tc_red", "tc_green", "tc_blue", "tc_gray",
            "term88_red", "term88_green", "term88_blue", "term88_gray"]

cmaps = [('Sequential',     create_colormaps())]




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

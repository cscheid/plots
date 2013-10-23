notes

- Categorical colormap needs different legend than continuous
  colormap, so colormaps need to know how to make their own legends

- color needs to be decoupled from shape, so that we can make size legends
  separately

- the decision on which legends to show needs to be made depending on which
  scales are not the default ones. This means we need a system of default
  scales.

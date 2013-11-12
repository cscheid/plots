notes

- Categorical colormap needs different legend than continuous
  colormap, so colormaps need to know how to make their own legends

- color needs to be decoupled from shape, so that we can make size legends
  separately

- the decision on which legends to show needs to be made depending on which
  scales are not the default ones. This means we need a system of default
  scales.


These ggplot2 examples work, and their equivalent should work here as
well.

> ggplot(iris, aes(Sepal.Width, Petal.Width)) + geom_point() + geom_hline(y=1.7)
> ggplot(iris, aes(Sepal.Width, Petal.Width)) + geom_point() + geom_hline(y=1.7) + coord_flip()
> ggplot(iris, aes(Sepal.Width, Petal.Width)) + geom_point() + geom_abline(slope=0, intercept=3)
> ggplot(iris, aes(Sepal.Width, Petal.Width)) + geom_point() + geom_abline(slope=0, intercept=1.7)

note that the coord_flip of a geom_abline is not a geom_abline, in the
same way that geom_hline -> geom_vline

> ggplot(iris, aes(Sepal.Width, Petal.Width)) + geom_point() + geom_abline(slope=0, intercept=1.7) + coord_flip()

A toy JPEG parser written in Haskell

## Executables

There are two executables generated:

`jpeg-to-pbm` converts a JPEG to a binary `.pgm` file if the file is grayscale or `.ppm` if it's color

``` text
jpeg-to-pbm - convert a jpeg file to pbm

Usage: jpeg-to-pbm FROM TO

Available options:
  -h,--help                Show this help text
```

`jpeg-info` prints the components in the header of a JPEG file

``` text
jpeg-info - print header info from a JPEG

Usage: jpeg-info IMAGE_FILE

Available options:
  -h,--help                Show this help text
```


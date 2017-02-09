# ngx-top

Monitor nginx access logs in real time.

## Installation

ngx-top binary can be built in two ways, either using [haskell stack tool](https://www.haskellstack.org):

```
stack build
```

The location of the produced binary can be found with `stack exec -- which ngx-top`.

Another option is to produce a fully statically linked binary using [docker-compose](https://docs.docker.com/compose/):

```
docker-compose up
```

and the binary will be placed in the `build` folder.

Statically linked binaries will be provided in the future via Release section.

## Usage

ngx-top logfile

## Performance

Using Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz ngx-top can currently process approximately 17000 log lines per second. Memory usage will vary depending on the log file, no extensive testing has been performed yet to determine accurate figures.
It's important to note that due to continuous processing memory usage will constantly grow overtime, to prevent an unintentional memory exhaustion ngx-top will exit when total memory usage reaches 256 MB.

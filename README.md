# ngx-top

Monitor nginx access logs in real time.

![screenshot](https://raw.githubusercontent.com/pbogdan/ngx-top/master/screenshots/screenshot.png)

![Build status](https://travis-ci.org/pbogdan/ngx-top.svg?branch=master)

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

Statically and dynamically linked binaries are now available via Releases section.

## Usage

```
$ ngx-top logfile
```

Currently ngx-top supports log files in the default combined format and a custom gateway format. At this time only the custom format contains all the required information to enable full current feature set.  
ngx-top will attempt to automatically detect correct log file format, defaulting to gateway format if auto-detection fails for any reason.

### Running a demo

ngx-top-gen binary is included to generate random log lines for testing / demo purposes:

```
stack exec -- ngx-top-gen > logfile
```

This will generate approximately 100 lines per second and write them to `logfile`.

And in another terminal window:

```
stack exec -- ngx-top logfile
```

`stack exec` invocations above can be replaced with usage of binaries in the build folder if docker-compose build method was used.

## Performance

Using Intel(R) Core(TM) i7-5500U CPU @ 2.40GHz ngx-top can currently process approximately 17000 log lines per second. Memory usage will vary depending on the log file, no extensive testing has been performed yet to determine accurate figures.
It's important to note that due to continuous processing memory usage will constantly grow overtime, to prevent an unintentional memory exhaustion ngx-top will exit when total memory usage reaches 256 MB.

---

This project includes GeoLite2 data created by MaxMind, available from [www.maxmind.com](http://www.maxmind.com).

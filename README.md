## Rolltime Forecast
Micro-service that predicts the availability of bikes and docks on many bike-sharing programs around the world. This repository is a concept repository and is **work-in-progress**.

[![](https://badge.imagelayers.io/luiscape/rolltime-forecast:latest.svg)](https://imagelayers.io/?images=luiscape/rolltime-forecast:latest 'Get your own badge on imagelayers.io')

## Usage
The forecast API has a single utility endpoint: `/forecast`. It takes a `station` parameter as argument.

```shell
$ curl localhost:6000/forecast?station=72
```

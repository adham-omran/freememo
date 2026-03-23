# How to build for prod

## Uberjar (Jetty 10+)

```shell
clojure -X:prod:build uberjar :version '"'$(git rev-parse HEAD)'"' :build/jar-name '"freememo.jar"'
java -cp target/freememo.jar clojure.main -m prod
```

## Uberjar (Jetty 9)

```shell
clojure -X:jetty9:prod:build uberjar :version '"'$(git rev-parse HEAD)'"' :shadow-build :prod-jetty9 :aliases '[:jetty9 :prod]' :build/jar-name '"freememo.jar"'
java -cp target/freememo.jar clojure.main -m prod-jetty9
```

## Docker

```shell
docker build --build-arg VERSION=$(git rev-parse HEAD) -t freememo:latest .
docker run --rm -it -p 8080:8080 freememo:latest
```

## Fly

```shell
fly deploy --remote-only --build-arg VERSION=$(git rev-parse HEAD)
```

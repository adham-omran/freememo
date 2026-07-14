FROM clojure:temurin-17-tools-deps-1.12.0.1501 AS build
WORKDIR /app
COPY deps.edn deps.edn
ARG VERSION
ENV VERSION=$VERSION
ARG GIT_COMMIT
ENV GIT_COMMIT=$GIT_COMMIT
RUN clojure -A:prod -M -e ::ok       # preload – rebuilds if deps or commit version changes
RUN clojure -A:build:prod -M -e ::ok # preload

COPY shadow-cljs.edn shadow-cljs.edn
COPY src src
COPY src-prod src-prod
COPY src-build src-build
COPY resources resources

RUN clojure -X:prod:build uberjar :version "\"$VERSION\"" :git-commit "\"$GIT_COMMIT\"" :build/jar-name "app.jar"

FROM eclipse-temurin:17-jre AS app
WORKDIR /app
# ImageMagick + libheif provide HEIC/HEIF decode for Live Document photo
# uploads — the JVM's ImageIO has no HEIC codec, so /api/heic-preview shells
# out to `magick`. (Base switched from amazoncorretto:17 / Amazon Linux 2,
# whose ImageMagick package lacks a reliable HEIF delegate.)
# curl backs the compose healthcheck.
RUN apt-get update \
 && apt-get install -y --no-install-recommends imagemagick libheif1 curl \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/target/app.jar app.jar

EXPOSE 8080
CMD java -cp app.jar clojure.main -m prod
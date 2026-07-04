# Ontop SPARQL endpoint over the cardmaker Postgres (read-only VKG facade).
# The base image ships no JDBC drivers; bundle the Postgres driver at the
# path the launcher scans. ADD leaves the jdbc/ dir root-owned without +x,
# unreadable to the non-root `ontop` (uid 999) runtime user — fix perms.
FROM ontop/ontop:5.3.0
USER root
ADD --chmod=644 https://repo1.maven.org/maven2/org/postgresql/postgresql/42.7.3/postgresql-42.7.3.jar /opt/ontop/jdbc/postgresql-42.7.3.jar
RUN chmod 755 /opt/ontop/jdbc
USER ontop

# Small, production JRE image
FROM eclipse-temurin:21-jre-alpine

WORKDIR /app
# copy your built jar (adjust the name/path if different)
COPY mock-server.jar /app/mock-server.jar

# Railway sets PORT; make sure your server binds to it
ENV PORT=8080

EXPOSE 8080
ENTRYPOINT ["java", "-jar", "/app/mock-server.jar"]

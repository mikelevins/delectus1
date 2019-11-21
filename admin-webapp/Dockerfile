FROM openjdk:8-alpine

COPY target/uberjar/admin-webapp.jar /admin-webapp/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/admin-webapp/app.jar"]

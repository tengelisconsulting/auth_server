#!/bin/sh

docker-compose up -d db
sleep 1
docker-compose up -d postgrest
sleep 1
docker-compose up -d auth_server
sleep 1
docker-compose up -d api_server
sleep 1
docker-compose up -d nginx

docker-compose logs --follow

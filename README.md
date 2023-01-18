# alexa-api

## Description
it provides restful interface for alexa-remote-control
- [alexa-remote-control](https://github.com/thorsten-gehrig/alexa-remote-control)

## Launch
### Using Docker-hub
most easy.
You don't even need to clone this repository

```
docker pull blowbranch/arc-rest
```

### Using Docker
```
cd production
docker compose up -d
```

### Launch locally
```
stack run
```

## Usage
see also [alexa-remote-control](https://github.com/thorsten-gehrig/alexa-remote-control).  
first, you need to setup environment variables by using ``/setup`` API.  

```
curl -X 'PUT' \
  'http://localhost:8080/setup' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
  "alexa": "alexa.amazon.co.jp",
  "amazon": "amazon.co.jp",
  "email": "your_email_address@example.com",
  "language": "ja_JP",
  "mfaSecret": "CD3C YCEF SIEF FOIE FC3F SD9C 8FLS ELFC KLJ3 HEC9 OSE3 AEFE 4FEC",
  "password": "your_password"
}'
```

then, you can run ``speak`` command api
```
curl -X 'POST' \
  'http://localhost:8080/speak' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
  "content": "Hello world!"
}'
```

you can also run ``command`` api
```
curl -X 'POST' \
  'http://localhost:8080/command' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
  "command": "singasong"
}'
```



## API Reference
you can also use swagger to read full api reference.
```
stack build
stack exec stack exec arc-rest-docs -- swagger.yaml    
```

## develop
### using docker 
edit source in local.
run this to start.
```
cd develop
docker-compose up
```
any PR is welcome.
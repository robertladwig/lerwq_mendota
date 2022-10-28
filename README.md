# LakeEnsemblR-WQ-rocker
Docker image for running Rstudio 3.6.3 with LakeEnsemblR.WQ and associated packages installed. This docker runs Ubuntu 18.04.

Currently includes:
- GLM-AED2
- Simstrat-AED2
- GOTM-Selmaprotas
- GOTM-WET  

Some commands to deal with Docker:
```{r gh-run, eval =FALSE}
# build a new container from current DOCKERFILE
sudo docker build -t lerwq .
# download the docker
docker pull hydrobert/lerwq:latest
# run the installed docker container
sudo docker run --rm -ti -e PASSWORD=password -p 8000:8000 lerwq
# stop all containers
docker kill $(docker ps -q)
docker rm $(docker ps -a -q)
# see all installed containers, first find "IMAGE ID"
docker images -a
docker rmi "IMAGE ID"
# purge everything docker-related
docker system prune --volumes
```

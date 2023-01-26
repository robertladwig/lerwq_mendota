# LakeEnsemblR-WQ-rocker
Docker image for running Rstudio 3.6.3 with LakeEnsemblR.WQ and associated packages installed. This docker runs Ubuntu 18.04.

Currently includes:
- GLM-AED2
- Simstrat-AED2
- GOTM-Selmaprotas
- GOTM-WET  

Planned:
- PCLake

Some commands to deal with Docker:
```{r gh-run, eval =FALSE}
# build a new container from current DOCKERFILE
sudo docker build -t lerwq .
# download the docker
docker pull hydrobert/lerwq:latest
# run the installed docker container
sudo docker run --rm -ti -e PASSWORD=password -p 8000:8000 hydrobert/lerwq
# run the installed docker container and link to volume
sudo docker run --rm -ti -v /home/robert/Projects/LER_docker/lerwq_mendota/calibration:/home/rstudio/calibration -e PASSWORD=password -p 8000:8000 hydrobert/lerwq
# stop all containers
docker kill $(docker ps -q)
docker rm $(docker ps -a -q)
# see all installed containers, first find "IMAGE ID"
docker images -a
docker rmi --force "IMAGE ID"
# purge everything docker-related
docker system prune --volumes
```

## Observed field data
[Physics](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.29.34)
[Chemistry](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.2.36)
[Phytoplankton](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.88.31)
[Nutrients](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.1.57)
[Zooplankton](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.90.33)

# full-cluster-cleanup

This is a helper script that will:

* Stop all running services
* Remove all the unmanaged containers on all cluster nodes
* Remove all downloaded images

> Setting up `DOCKER_HOST` variable is enough for this script to detect all the
> participating nodes of the swarm cluster.

> At this point this script will not attempt to remove any of the overlay
> networks or named volumes.

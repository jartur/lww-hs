# lww-hs

[![Build Status](https://travis-ci.org/jartur/lww-hs.svg?branch=master)](https://travis-ci.org/jartur/lww-hs)

Implementation of LWW-Element-Set (state based) in Haskell & a distributed service built around it.

# Docs

## API

| Method | Path | Payload | Response | Comment
| ------ | ---- | ------- | -------- | -------
| GET     |`/<set>` |       | Set as JSON | 
| GET | `/<set>/<elem>` | | true/false for presense | 
| POST | `/<set>` | Set as JSON | null | Merges the complete set, used for cluster activities as well
| PUT | `/<set>/<elem>` | None expected | null | Will create specified element †
| DELETE | `/<set>/<elem>` | | null | Removes element from the set
  
† In retrospective probably should be at path `/<set>` and accept the element in payload.

## Library

https://jartur.github.io/lww-hs/lww-hs-0.1.0.0/LWWSet.html

# Architecture

The main application is a REST service, that uses a local SQLite for persistence. 

Every node is completely independent and can be run anywhere. There is no centralized 
data storage. You can (and should) run multiple nodes providing them a list of some 
of the other nodes so that they can join the cluster and discover all of the nodes.

The cluster structure is stored in the LWWSet `_peers_`.

You can configure how often the partial replication and full replication will be run.

Because replicating an LWWSet that contains only recent local updates is the same 
as replicating all those updates themselves we achieve a basically free batching for 
partial replication.

Full replication is implemented not very efficiently, but this is one of those things that
would take more investigation to design properly.

# Scaling

In the current form it is possible to scale out to many nodes, which are independent, provided one sets partial replication to occur at relatively large delays and full replication to be very rare. 
Of course, with low replication we sacrifice consistency and make it more and more eventual.

All-to-all replication will cause a cascade of replication, which is inefficient, and is the main limiting factor to scaling.

In general, we need to improve the replication process to achieve higher scalability. For example
we could push hashes of sets and then only replicate the changed sets to the nodes which have outdated versions. We could try to build some topology discovery and dynamically construct replication
strategies that would avoid all-to-all replication. 

We can turn it into a database where not all of the nodes hold all of the data, which will make it 
much more scalable (including space, because full replication means we cannot scale in terms of space by adding more nodes). Something based on consistent hashing may work, although we will have to identify nodes in some reasonable way, maybe in that case it's nicer to have a stable topology, something like Cassandra. 

Riak also may be a good source of ideas.

# Deployment 

Unfortunately I didn't have time to create any deployment scripts / containers. But I will describe here the approach I would take.

There are few approaches how to provision the environment on the target servers. One of them is using ansible and other is using something like puppet or chef. In my experience, ansible is much nicer for development, because it's very easy to change things in an ad hoc way. Puppet/chef may be nicer for a production environment because it's very easy to keep a huge amount of machines in the same state in a centralized way.

If we are in the cloud (AWS, etc) it may be worth to have prebuild images to instantiate instead.

As for the deployment of the application itself, I would, in this case, prefer docker container for the app. In general, when I am working with JVM, I deploy the jars and dependencies directly to the target machine, because it is very easy to provision for JVM, you just need a JRE and all the dependenicies are local to the application. With Haskell (and any other language without a VM and poitentially global dependencies) it would be preferable to put the final app with all the necessary dependencies inside a docker container. 

For instance, something like this:

* On the build server (say Team City) build the app inside a docker container (there's even support for that in Stack)
* Prepare image for deployment with all the runtime dependencies
* Generate docker image the built binaries (ther's also support for this in Stack) based on the previous one
* Push the image to private repo
* With ansible fetch the image at the target hosts and run the new version

# Buildchain

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

On MacOs you can also just `brew install haskell-stack`

# Build

```
stack install
```

# Running

Run any command with `--help` to see the list of options.

## Main app

```
~/.local/bin/lww-hs-app -p 3000 -d sqlite3_3000.db -i localhost:3000 -i localhost:3001 -l -r 5 -f 60
~/.local/bin/lww-hs-app -p 3001 -d sqlite3_3001.db -i localhost:3000 -i localhost:3001 -l -r 5 -f 60
```

Note that you can list as many initial peers as you want by repeating the option. 

*NB* if you want your node to receive replicas you must add its public address and port to the peers list.

## Monkeys

```
~/.local/bin/lww-hs-monkey-normal -a host:port --setname=superSet
~/.local/bin/lww-hs-monkey-onoff -a host:port --setname=onoffMe
~/.local/bin/lww-hs-monkey-viewer -a host:port -s _peers_
```

# Lessons learned

It has been an intense experience, because while I have been using Haskell for quite some time to solve programming challenges and writing small simple tools this one is my first _real_ application so I had to spend a loooot of time to actually work out how to use Scoty + Database backend + manage state all throughout. 

Sometimes it was just an excersise in futility, when typechecker screams at you something completely _nonsensical_ and you are just staring at the screen, almost crying inside. "I don't understand what do you want from me!". But then again and again I conquered the issues, thus this actually kinda works.

Now, I bit more than I can chew, so I had spend way too much time on battling with the unfamiliar (turns out Haskell is still hard if you don't practice it all the time) instead of improving the solution itself. It's very unfortunate because I had the idea how to solve this problem very early into the development, and had I just chosen Java or Kotlin or Scala or even Clojure, I could have done so much more.

That is a good lesson, but also I am happy that I have broken the walls I had encountered and learned a lot in a rather short time. 

# TODO

* QuickCheck testing can be improved by writing an `Arbitrary` instance for the datastructure (e.g. to check for commutativity and associativity).
* Better replication strategies. 
* Replicate to subset of nodes in the cluster. Maybe some topology discovery.
* Some solution to select which nodes to use as primary for a particular set. (Maybe consistent hashing could help here?)
* Nodes that only hold some of the sets, so we actually can scale out, basically building a kind of a Cassandra cluster with replication factors. Although with complete peer to peer architecture it may be more difficult or impossible.
* Proper error handling.
* Docker deployment
* Don't use timestamps? Wall clock is super unreliable, especially when we can have a local copy on offline client. Maybe vector clock? Need to investigate the options here.
* And so much more

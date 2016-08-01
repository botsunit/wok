.PHONY: doc docker-compose.yml images/wok.deps.png images/wok.call.png
include bu.mk

compile-erl:
	$(verbose) $(REBAR) compile

compile-ex: elixir
	$(verbose) $(MIX) deps.get
	$(verbose) $(MIX) compile

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

tests:
	$(verbose) $(REBAR) eunit

doc: images/wok.call.png images/wok.deps.png _doc/doc.yml
	$(verbose) $(REBAR) as doc edoc
	$(verbose) ${MKDIR_P} doc/images
	$(verbose) ${CP} images/*.png doc/images
	$(verbose) ${CP} _doc/* doc

images/wok.call.png: images/wok.call.gv
	$(verbose) dot -T png -o images/wok.call.png images/wok.call.gv

images/wok.deps.png: images/wok.deps.gv
	$(verbose) dot -T png -o images/wok.deps.png images/wok.deps.gv

dist-erl: clean-erl compile-erl tests

dist-ex: clean-ex compile-ex

dist: dist-erl dist-ex doc

clean-ex:
	$(verbose) $(RM_RF) _build deps

clean-erl:
	$(verbose) $(RM_RF) _build test/eunit

clean: clean-ex clean-erl

distclean: clean-ex clean-erl
	$(verbose) rm -f rebar.lock mix.lock

dev: compile
	$(verbose) erl -pa _build/default/lib/*/ebin _build/default/lib/*/include -config config/wok.config

KAFKA_ADVERTISED_HOST_NAME = $(shell ip addr list docker0 |grep "inet " |cut -d' ' -f6|cut -d/ -f1)

define docker_compose_yml_v1
version: "2"

services:
  zookeeper:
    image: dockerkafka/zookeeper
    ports:
      - "2181:2181"

  kafka1:
    image: wurstmeister/kafka
    ports:
      - "9092:9092"
    links:
      - zookeeper:zk
    environment:
      KAFKA_ZOOKEEPER_CONNECT: zk
      KAFKA_BROKER_ID: 1
      KAFKA_ADVERTISED_HOST_NAME: $(KAFKA_ADVERTISED_HOST_NAME)
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock

  kafka2:
    image: wurstmeister/kafka
    ports:
      - "9093:9092"
    links:
      - zookeeper:zk
    environment:
      KAFKA_ZOOKEEPER_CONNECT: zk
      KAFKA_BROKER_ID: 2
      KAFKA_ADVERTISED_HOST_NAME: $(KAFKA_ADVERTISED_HOST_NAME)
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock

  kafka3:
    image: wurstmeister/kafka
    ports:
      - "9094:9092"
    links:
      - zookeeper:zk
    environment:
      KAFKA_ZOOKEEPER_CONNECT: zk
      KAFKA_BROKER_ID: 3
      KAFKA_ADVERTISED_HOST_NAME: $(KAFKA_ADVERTISED_HOST_NAME)
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock

  tools:
    image: confluent/tools
    depends_on:
      - zookeeper
      - kafka1
      - kafka2
      - kafka3
endef

docker-compose.yml:
	$(call render_template,docker_compose_yml_v1,docker-compose.yml)

docker-start: docker-stop
	$(verbose) docker-compose up -d
	$(verbose) sleep 1
	$(verbose) docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic test
	$(verbose) docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic repl
	$(verbose) docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic public
	$(verbose) docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic service

docker-stop: docker-compose.yml
	$(verbose) docker-compose kill
	$(verbose) docker-compose rm --all -vf


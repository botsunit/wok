PROJECT = wok

.PHONY = images/wok.deps.png images/wok.call.png

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
ELIXIR_BINDINGS = wok wok_request wok_response wok_routes wok_message

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = lager wok_http_adapter wok_message_handler wok_producer pipette kafe cowboy cowboy_default_static_file bucs doteki uuid tempfile

dep_lager = git https://github.com/basho/lager.git 3.2.0
dep_wok_http_adapter = git git@gitlab.botsunit.com:msaas/wok_http_adapter.git 0.0.4
dep_wok_message_handler = git git@gitlab.botsunit.com:msaas/wok_message_handler.git 0.2.1
dep_wok_producer = git git@gitlab.botsunit.com:msaas/wok_producer.git 0.2.1
dep_pipette = git git@gitlab.botsunit.com:msaas/pipette.git 0.0.1
dep_kafe = git https://github.com/botsunit/kafe.git 1.3.0
dep_cowboy_default_static_file = git https://github.com/botsunit/cowboy_default_static_file.git 1.1.0
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.0.0-pre.3
dep_bucs = git https://github.com/botsunit/bucs.git 0.0.1
dep_doteki = git https://github.com/botsunit/doteki.git 0.0.1
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git v0.5.0
dep_tempfile = git https://github.com/botsunit/tempfile.git 1.0.1

DOC_DEPS = edown

dep_edown = git https://github.com/botsunit/edown.git master

TEST_DEPS = meck wok_tests

dep_meck = git https://github.com/eproxus/meck.git master
dep_wok_tests = git git@gitlab.botsunit.com:msaas/wok_tests.git 0.1.1

CP = cp
CP_R = cp -r
RM_RF = rm -rf
MKDIR_P = mkdir -p
DATE = $(shell date +"%F %T")

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {edown_target, gitlab} \
						, {top_level_readme, {"./README.md", "https://gitlab.botsunit.com/msaas/${PROJECT}"}}

EUNIT_OPTS = verbose, {report, {eunit_surefire, [{dir, "test"}]}}

include erlang.mk

docs:: edoc images/wok.call.png images/wok.deps.png _doc/doc.yml
	@${MKDIR_P} doc/images
	@${CP} images/*.png doc/images
	@${CP} _doc/* doc

images/wok.call.png: images/wok.call.gv
	@dot -T png -o images/wok.call.png images/wok.call.gv

images/wok.deps.png: images/wok.deps.gv
	@dot -T png -o images/wok.deps.png images/wok.deps.gv

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/${PROJECT}.config

release: app mix.all

KAFKA_ADVERTISED_HOST_NAME = $(shell ip addr list docker0 |grep "inet " |cut -d' ' -f6|cut -d/ -f1)

define docker_compose_yml
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
	$(call render_template,docker_compose_yml,docker-compose.yml)

docker-start: docker-stop
	@docker-compose up -d
	@sleep 1
	@docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic test
	@docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic repl
	@docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic public
	@docker-compose run --rm tools kafka-topics --create --zookeeper zookeeper:2181 --replication-factor 3 --partitions 3 --topic service

docker-stop: docker-compose.yml
	@docker-compose kill
	@docker-compose rm --all -vf


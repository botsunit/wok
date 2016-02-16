# Configuration

## Messages configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `wok`/`messages`| `handler`| Handler used to encode/decode kafka messages |
| `wok`/`messages`| `controlers`| List of controlers used  to consume messages |
| `wok`/`messages`| `consumer_group`| |
| `wok`/`messages`| `local_queue_name`| |
| `wok`/`messages`| `local_consumer_group`| |
| `wok`/`messages`| `max_services_fork`| |
| `wok`/`messages`| `topics`| |

## Initializer configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `wok`/`initializer`| | Initialiser configuration |

## Middlewares configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `wok`/`middlewares`| | List of middelwares configurations |

## REST configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `wok`/`rest`| `port` | Port used for the HTTP REST server |
| `wok`/`rest`| `ip` | IP used for the HTTP REST server |
| `wok`/`rest`| `max_conn` | Maximum number of connections |
| `wok`/`rest`| `routes` | List of routes |

## Kafe (kafka) configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `kafe` | `brokers` | |
| `kafe` | `client_id` | |
| `kafe` | `api_version` | |
| `kafe` | `correlation_id` | |

## Pipette configuration

| Namespace | Key | Value |
|:----------|:----|:------|
| `pipette` | `segment_path` | |
| `pipette` | `segment_memory_size` | |
| `pipette` | `segment_file_size` | |
| `pipette` | `segment_flush_frequency` | |
| `pipette` | `retention_hours` | |
| `pipette` | `retention_check_interval` | |


-record(msg, {
          uuid,
          from,
          to,
          headers,
          body,
          params = #{},
          offset,
          key,
          message,
          topic,
          partition
         }).

-record(wok_message, {
          request = #msg{},
          response = #msg{},
          global_state = undefined,
          local_state = undefined,
          custom_data = #{},
          reply = false,
          action
         }).


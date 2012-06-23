{application,bot,
             [{description,[]},
              {vsn,"1"},
              {registered,[bot_sup,but_wrk]},
              {applications,[kernel,stdlib,exmpp,inets]},
              {mod,{bot_app,[]}},
              {env,[]},
              {modules,[bot_app,bot_sup,bot_wrk]}]}.

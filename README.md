# hzerl

Erlang driver for Hazelcast Data Grid. 
hzerl written on Erlang and Clojure with using the Jinterface. 

http://www.hazelcast.com/

## Usage

```erlang


hzerl:start_link().

Config = {user, UserName,
                password, UserPassword,
                hosts, ["localhost", "10.1.1.1"]
                connAtemptLimit, 5 , %% unlimited by default
                connAtemptPeriod, 3000,
                connTimeout, 5000}.

hzerl:connect(Config).

%%% Async operations

hzerl:hz_cast([map, "custom", "SET", "1", "b"]).

%%% Sync operations 

hzerl:hz_call([map, "custom", 'GET', "1"]).

```


## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

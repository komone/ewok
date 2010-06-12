%% Universal Service Protocol
%% usp.hrl

-record(usp_type, {id, term, contraints = []}).

-record(usp_operation, {call, in = [], out = [], start, next}).

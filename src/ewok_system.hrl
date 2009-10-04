%% ewok_system.hrl
%% These are records that need to be shared between services but they should
%% not be required by the API. Records required by the API are in ewok.hrl.

%%
-record(server, {runmode, dir, apps=[]}).
%%
-record(web_app, {id, path, valid=false, deployed=false}).
%%
-record(datasource, {name, id, interface, running=false, valid=false, data=[]}).
%%
-record(log, {id, fd, path, rollover=infinity, maxfiles=infinity}).

%%
-record(workflow, {id, name, tasks=[]}).

%% TODO: a 'task' record for workflow would conflict with the scheduler's task record
-record(task2, {id, name, roles, more}). 

%% ewok_system.hrl

-record(server, {runmode, dir, apps=[]}).
-record(web_app, {id, path, valid=false, deployed=false}).
-record(datasource, {name, id, interface, running=false, valid=false, data=[]}).
-record(log, {id, fd, path, rollover=infinity, maxfiles=infinity}).

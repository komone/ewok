%% UMTP Messages

-record(time, {offset=0}).
-record(dns, {key="A", value='_'}).
-record(whois, {key, value='_'}).

-record(mail, {from, to=[], body=[]}).
-record(web, {headers=[], body=[]}).

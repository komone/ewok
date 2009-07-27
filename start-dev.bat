@echo off
echo Ewok/1.0 (Wicket)
erl -sname ewok -pa ./ebin -mnesia dir '"priv/data/mnesia"' -boot start_sasl -s reloader -s ewok
@pause
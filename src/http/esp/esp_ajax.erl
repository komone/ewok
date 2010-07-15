%%
-module(esp_ajax).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-compile(export_all).

%% NOTE: This module may be removed/deleted!

%% NOTE: EXPERIMENTAL. The currently preferred approach is to use records from esp.hrl 
%% that are preprocessed to generate html with class tags that are operated on by 
%% JQuery functions defined in the main esp-{version}.js javascript

%% NOTE: For the rationale of this approach to ajax, see Joe's post/thoughts on a 
%% "composable web application", originally posted to the Nitrogen Google group.

%% To generate function bodies for this module from javascrpt development sources, 
%% see: ewok_js.erl:compile_ajax/2

%% IMPL: when adding new controls to this module, we must also add a custom 
%% record to esp.hrl and a custom tag function call to esp.erl that processes
%% the js_binary returned and accumulates it into a script in the html page.
%%
%% First create the working javascript control, then use esp_ajax:compile_js
%% to generate the <control>.js.src file which contains the parameterized 
%% list that may be copy/pasted into the new esp_ajax function call.

%% First example: JS for inplace Textbox
%% This could be called directly by apps, but we strongly recommend the use 
%% of the corresponding record(s) in esp.hrl which allows users to access and
%% manage arguments more easily. i.e.
%% #inplace{id=myid, edit="edit", save="save"}).
inplace(ID) ->
	inplace(ID, <<"edit">>, <<"save">>).
inplace(ID, Edit, Save) -> 
	IDString = atom_to_binary(ID, utf8),
	list_to_binary([
		<<"$(function () {">>,
		<<"$(\"#">>,IDString,<<"-ctrl\").click(function () {">>,
		<<"if ($(this).html() == \"">>,Edit,<<"\") {">>,<<"$(\"#">>,IDString,
		<<"-val\").html(">>,<<"\"<input id='">>,IDString,
		<<"-input' type='text' value='\" ">>,<<"+ $(\"#">>,IDString,
		<<"-val\").html() + \"'/>\");">>,<<"$(this).html(\"">>,Save,<<"\");">>,
		<<"} else {">>,<<"var value = $(\"#">>,IDString,
		<<"-input\").attr(\"value\");">>,<<"$(\"#">>,IDString,
		<<"-val\").html(value);">>,<<"var result = {">>,IDString,<<": value};">>,
		<<"$.ajax({">>,<<"url: \"/json\",">>,<<"type: \"POST\",">>,
		<<"contentType: \"application/json\",">>,<<"data: result.toJSONString(), ">>,
		<<"   ">>,<<"success: function(response) {">>,
		<<"$(\"#response4\").text(response.toJSONString());">>,<<"   ">>,<<"},">>,
		<<"   ">>,<<"error: function(data) {">>,<<"$(\"#response4\").text(data);">>,
		<<"}">>,<<"});">>,<<"$(this).html(\"">>,Edit,<<"\");">>,<<"}">>,
		<<"}).hover(">>,<<"function(){$(this).css(\"cursor\", \"pointer\")},">>,
		<<"function(){$(this).css(\"cursor\", \"auto\")}">>,<<");">>,
		<<"});">>
	]).

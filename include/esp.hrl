%%
%% File: esp.hrl
%% Version: 1.0.0 beta
%% Author: Steve Davis <steve@simulacity.com>
%% Updated: July 30, 2009
%%

%% ESP component record definitions for use in applications

%% NOTE: The ESP implementation enables "composable web applications" as outlined by
%% Joe Armstrong in the Nitrogen Google Group. Ref: ./doc/joe.txt as I can't find the 
%% original posting on google groups any more).

%% NOTE: Most records has a 'fields' field that allows ESP to identify, at runtime, the
%% record fields by name tags/ids. i.e. the 'fields' field defines record_info() for runtime
%% use by the ESP processor. In usage, the 'fields' can, should, and MUST be ignored by 
%% application developers. Changing the value of the 'fields' field in application code
%% *will cause processing errors*. You have been warned!
%% Application code should thus look like, e.g.: #title{body="My Website Home"}, etc.

%% IMPL: In general, we try to limit the use of xhtml id and class attributes as
%% much as possible; and we try to rely on CSS/JQuery xpath/selectors as much as 
%% possible. This has the effect that we can generate far cleaner JS/AJAX code and 
%% more 'grockable' markup from ESP.

%%
-record(page, {doctype=xhtml, title, head=[], body=[]}).
%% Deletable?
%-record(esp, {page=[], handler, request, session}).
%% Deletable?
%-record(skin, {callbacks=[]}). %% ???

%%
%% HTML/XHTML Elements
%%
-record(html,   {fields=[body], body}).
-record(head,   {fields=[body], body}).
-record(title,  {fields=[body], body}).
-record(meta,   {fields=['http-equiv', content], 'http-equiv', content}).
-record(link,   {fields=[rel, href, type, media, charset], rel, href, type, media, charset}).

%% NOTE: "There is no need to use the language or type attributes. It is the 
%%  server, not the script tag, that determines the MIME type." 
%%  <http://javascript.crockford.com/code.html> 
-record(script, {fields=[src], src}).

-record(body,   {fields=[body], body}).

-record('div',  {fields=[id, class, body], id, class, body}).
-record(span,   {fields=[id, class, body], id, class, body}).

-record(h1,     {fields=[body], body}).
-record(h2,     {fields=[body], body}).
-record(h3,     {fields=[body], body}).
-record(h4,     {fields=[body], body}).
-record(h5,     {fields=[body], body}).

-record(p,      {fields=[class, body], class, body}).
-record(br,     {fields=[clear], clear}).
-record(hr,     {fields=[]}).

-record(pre,    {fields=[body], body}).
-record(ul,     {fields=[body], body}).
-record(li,     {fields=[body], body}).

-record(a,      {fields=[href, body], href, body}).
-record(img,    {fields=[id, src], id, src}).

-record(table,    {fields=[class, body], class, body}).
-record(caption,  {fields=[body], body}).
-record(colgroup, {fields=[body], body}).
-record(col,      {fields=[body], body}).
-record(thead,    {fields=[body], body}).
-record(tbody,    {fields=[body], body}).
-record(tfoot,    {fields=[body], body}).
-record(tr,       {fields=[class, body], class, body}).
-record(th,       {fields=[body], body}).
-record(td,       {fields=[class, body], class, body}).

-record(form,     {fields=[method, action, body], method=post, action, body}). %%
-record(input,    {fields=[label, type, name, class, value], label, type=text, name, class, value}).
-record(textarea, {fields=[name, body], name, body=[]}).

%%
%% ESP Custom Components and AJAX Controls
%% 
-record(css,      {fields=[src, type, media, charset], src, type="text/css", media="screen", charset="utf-8"}).
-record(grid,     {fields=[caption, header, body, footer, opts], caption, header=[], body=[], footer=[], opts=[]}).
-record(inplace,  {fields=[id, label, value, edit, save, message], id, label=[], value=[], edit="edit", save="save", message=[]}).
%% more to come...

%% end %%

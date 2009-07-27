% ewok - esp.hrl

-record(page, {doctype=xhtml, title, head=[], body=[]}).
%-record(esp, {page=[], handler, request, session}).
-record(skin, {callbacks=[]}). %% ???

-record(html,   {fields=[body], body}).
-record(head,   {fields=[body], body}).
-record(title,  {fields=[body], body}).
-record(meta,   {fields=['http-equiv', content], 'http-equiv', content}).
-record(link,   {fields=[rel, href, type, media, charset], rel, href, type, media, charset}).
%% if the world were rid of MSIE then we could safely ignore incorrect use of "text/javascript".. see ewok_http_compatibility
-record(script, {fields=[src, language, type], src, language="JavaScript", type="application/javascript"}).

-record(body,   {fields=[body], body}).

-record('div', {fields=[id, class, body], id, class, body}).
-record(span,  {fields=[id, class, body], id, class, body}).

-record(h1,    {fields=[body], body}).
-record(h2,    {fields=[body], body}).
-record(h3,    {fields=[body], body}).
-record(h4,    {fields=[body], body}).
-record(h5,    {fields=[body], body}).

-record(p,     {fields=[body], body}).
-record(br,    {fields=[clear], clear}).
-record(hr,    {fields=[]}).

-record(pre,   {fields=[body], body}).
-record(ul,    {fields=[body], body}).
-record(li,    {fields=[body], body}).

-record(a,     {fields=[href, body], href, body}).
-record(img,   {fields=[id, src], id, src}).

-record(table,    {fields=[class, body], class, body}).
-record(caption,  {fields=[body], body}).
-record(colgroup, {fields=[body], body}).
-record(col,      {fields=[body], body}).
-record(thead,    {fields=[body], body}).
-record(tbody,    {fields=[body], body}).
-record(tfoot,    {fields=[body], body}).
-record(tr,       {fields=[body], body}).
-record(th,       {fields=[body], body}).
-record(td,       {fields=[body], body}).

-record(form,     {fields=[method, action, body], method=post, action, body}). %%
-record(input,    {fields=[label, type, name, value], label, type=text, name, value}).
-record(textarea, {fields=[name, body], name, body=[]}).

%% esp components
-record(css,   {fields=[path, type, media, charset], path, type="text/css", media="screen", charset="utf-8"}).
-record(grid,  {fields=[caption, header, body, footer, opts], caption, header=[], body=[], footer=[], opts=[]}).

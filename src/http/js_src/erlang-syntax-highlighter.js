/**
 * Code Syntax Highlighter.
 * Version 1.5.2
 * Copyright (C) 2004-2008 Alex Gorbatchev
 * http://www.dreamprojections.com/syntaxhighlighter/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, version 3 of the License.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see http://www.gnu.org/licenses/.
 */

/*   
 * Erlang syntax contributed by Jean-Lou Dupont  http://www.jldupont.com/  
 * file: shBrushErlang.js
 */
dp.sh.Brushes.Erlang = function()
{
 // According to: http://erlang.org/doc/reference_manual/introduction.html#1.5
    var keywords = 'after and andalso band begin bnot bor bsl bsr bxor '+
        'case catch cond div end fun if let not of or orelse '+
        'query receive rem try when xor'+
        // additional
        ' module export import define';

    this.regexList = [
        { regex: new RegExp("[A-Z][A-Za-z0-9_]+",      'g'), css: 'vars' },
        { regex: new RegExp("\\%.+",                  'gm'), css: 'comment' },
        { regex: new RegExp("\\?[A-Za-z0-9_]+",        'g'), css: 'preprocessor' },
        { regex: new RegExp("[a-z0-9_]+:[a-z0-9_]+",   'g'), css: 'mod_func' },
        
        { regex: new RegExp('"(?!")(?:\\.|\\\\\\"|[^\\""\\n\\r])*"', 'gm'), css: 'string' },
        { regex: new RegExp("'(?!')(?:\\.|(\\\\\\')|[^\\''\\n\\r])*'", 'gm'), css: 'string' },

        { regex: new RegExp(this.GetKeywords(keywords), 'gm'), css: 'keyword' },
        ];

    this.CssClass = 'dp-erl';
    this.Style =    '.dp-erl .vars     { color: rgb(184,134,11); }' +
                    '.dp-erl .mod_func { color: #CC00FF; }';
};

dp.sh.Brushes.Erlang.prototype  = new dp.sh.Highlighter();
dp.sh.Brushes.Erlang.Aliases    = ['erl', 'erlang'];

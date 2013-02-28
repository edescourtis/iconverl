About
=====

This new version adds support for streaming. You can now have low level access to iconverl:iconv/2 in case you really need it or you can use the iconverl:chunk/2 function for a simpler interface. Currently only two implementations are recommended this one and eiconv. Backwards compatibility with erlang-iconv, the old iconverl and eiconv is also provided. So you can use this library as a drop in replacement for existing ones.

Note that this is a complete rewrite of iamaleksey/iconverl which has significant improvements.

Compilation
===========

<pre>
make
</pre>

Usage
=====

<pre>
1> CD = iconverl:open("ucs-2be", "utf-8").
&lt;&lt;&gt;&gt;
2> iconverl:conv(CD, &lt;&lt;"text"&gt;&gt;).
{ok,&lt;&lt;0,116,0,101,0,120,0,116&gt;&gt;}
3> iconverl:conv(CD, &lt;&lt;"more text to convert"&gt;&gt;).
{ok,&lt;&lt;0,109,0,111,0,114,0,101,0,32,0,116,0,101,0,120,0,
      116,0,32,0,116,0,111,0,32,0,...&gt;&gt;}
4> iconverl:conv("ucs-4", "latin1", &lt;&lt;"convert with a single function call"&gt;&gt;).
{ok,&lt;&lt;0,0,0,99,0,0,0,111,0,0,0,110,0,0,0,118,0,0,0,101,0,
      0,0,114,0,0,0,...&gt;&gt;}

</pre>

Or using the standard iconv interface (recommended):
<pre>
1&gt; l(iconv).<br />{module,iconv}<br />2&gt; {ok, Cd} = iconv:open("latin1//translit", "utf-8").<br />{ok,&lt;&lt;&gt;&gt;}<br />3&gt; iconv:conv(Cd, unicode:characters_to_binary("Test&forall;", utf8)).<br />{ok,&lt;&lt;"Test?"&gt;&gt;}<br />4&gt; iconv:close(Cd).<br />ok<br />5&gt;
</pre>




<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:r="http://www.r-project.org" version="1.0">
  <xsl:import href="html/defs.xsl"/>
  <xsl:import href="html/shell.xsl"/>
  <xsl:import href="html/xml.xsl"/>
  <xsl:param name="show.done" select="false"/>
  <xsl:key name="topic" match="items/item[@topic]" use="concat(parent::items/parent::topic/title, @status)"/>
  <xsl:variable name="topics" select="//item[@topic]"/>
  <xsl:variable name="topics-list" select="$topics[not(@topic = preceding::item/@topic)]/@topic"/>
  <!-- To get the first topic via a title <xsl:value-of select="/topics/topic/title/text()"/> -->
  <xsl:template match="/">
    <html>
      <head>
        <!-- Want to put in the name of the directory of the document in the title -->
        <!--        <xsl:value-of select="document-uri(/)"/> -->
        <title>Todo List: <xsl:value-of select="/topics/title"/></title>
        <link rel="stylesheet" href="Admin/CSS/OmegaTech.css"/>
        <link rel="stylesheet" href="Admin/CSS/Todo.css"/>
        <script language="JavaScript" src="Admin/JavaScript/tooltips.js"/>
        <script language="JavaScript" src="Admin/JavaScript/toggleHidden.js"/>
      </head>
      <body>
        <!--  Put out any top-level ulinks to other Todo files. -->
        <xsl:if test="/topics/ulink">
          <table>
            <tr>
              <xsl:for-each select="/topics/ulink">
                <th>
                  <xsl:apply-templates select="."/>
                </th>
              </xsl:for-each>
            </tr>
          </table>
          <hr width="50%"/>
        </xsl:if>
        <center>
          <table>
            <tr valign="top">
              <th align="left" style="padding-right: 130pt">
                <!-- Table of topics with their counts.  -->
                <ol>
                  <xsl:for-each select="/topics/topic">
                    <li><xsl:element name="a"><xsl:attribute name="href">#<xsl:value-of select="title"/></xsl:attribute><xsl:value-of select="title"/></xsl:element>
       - <xsl:value-of select="count(./items/item)"/><xsl:text> </xsl:text><i class="notDone">(<xsl:value-of select="count(./items/item[not(@status='done')])"/>)</i>
      </li>
                  </xsl:for-each>
                  <li>
                    <a href="#Done">Done</a>
                  </li>
                </ol>
              </th>
              <th align="right">
   Total number of elements: <xsl:value-of select="count(//item)"/>
<br/>
   Not yet completed: <xsl:value-of select="count(//item[not(@status='done') and not(@status='info')])"/>
<br/>
   No priority: <xsl:value-of select="count(//item[not(@status)])"/>
<br/>
   High-only priority: <xsl:value-of select="count(//item[@status='high'])"/>
<br/>
   Completed: <xsl:value-of select="count(//item[@status='done' or @status='info'])"/>
</th>
            </tr>
          </table>
        </center>
        <xsl:if test="count(/topics/topic) &gt; 1">
          <hr width="50%" height="2"/>
          <!-- Announce the different topics.  -->
          <!-- Show the summary tables for all topics -->
          <div class="count-tables">
            <xsl:call-template name="showCountTables"/>
          </div>
        </xsl:if>
        <div>
          <hr width="50%"/>
        </div>
<input id="ShowDone" type="button" value="ShowDone" onclick="toggleDone();">Show Done</input>
          <hr width="50%"/>
<div status="news">
<h2>News</h2>
<dl>
    <xsl:for-each select="//item[@status='notify']">
          <xsl:call-template name="item"/>
    </xsl:for-each>
</dl>
</div>
        <xsl:call-template name="make-topic-list">
          <xsl:with-param name="nodes" select="//item[not(@topic) and not(ancestor::topic)]"/>
          <xsl:with-param name="topic" select="General"/>
        </xsl:call-template>
        <!-- Display the elements for which we have an explicit @topic -->
        <xsl:for-each select="$topics-list">
          <xsl:variable name="topic" select="string(.)"/>
          <xsl:call-template name="make-topic-list">
            <xsl:with-param name="nodes" select="//item[@topic=$topic]"/>
            <xsl:with-param name="topic" select="$topic"/>
          </xsl:call-template>
        </xsl:for-each>
        <!-- Show the individual topics with summary table (again) and the individual items in a list. -->
        <xsl:apply-templates/>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="/topics/title"/>
  <xsl:template name="showCountTables">
    <table>
      <tr valign="top">
        <xsl:for-each select="$topics-list">
          <xsl:variable name="topic" select="string(.)"/>
          <th class="table-count">
            <div class="table-count">
              <xsl:call-template name="topicCountTable">
                <xsl:with-param name="nodes" select="//item[@topic=$topic]"/>
                <xsl:with-param name="topic" select="$topic"/>
              </xsl:call-template>
            </div>
          </th>
        </xsl:for-each>
        <xsl:for-each select="topics/topic">
          <th>
            <div border="1">
              <table border="0" frame="1">
                <tbody>
                  <!--
    <tr><th><xsl:element name="a"><xsl:attribute name="href">#<xsl:value-of select="title"/></xsl:attribute><xsl:value-of select="title"/></xsl:element></th></tr>
-->
                  <tr>
                    <th class="table-count">
                      <div class="table-count">
                        <xsl:call-template name="topicCountTable">
                          <xsl:with-param name="topic">
                            <xsl:value-of select="./title"/>
                          </xsl:with-param>
                        </xsl:call-template>
                      </div>
                    </th>
                  </tr>
                </tbody>
              </table>
            </div>
          </th>
        </xsl:for-each>
      </tr>
    </table>
  </xsl:template>
  <!--
  Need to find a way to use a different match="item" template.
<xsl:key name="TotalStatus" match="items/item[@status]" use="@status" />
-->
  <!-- Hint from http://www.dpawson.co.uk/xsl/sect2/N2018.html#d4e412 -->
  <xsl:template match="item" name="make-item">
    <tr>
      <xsl:element name="td">
        <xsl:attribute name="align">left</xsl:attribute>
        <xsl:attribute name="class">
          <xsl:value-of select="@status"/>
        </xsl:attribute>
        <xsl:value-of select="@status"/>
      </xsl:element>
      <td align="right">
        <xsl:value-of select="count(key('status', concat(../../title, @status)))"/>
      </td>
    </tr>
  </xsl:template>
  <xsl:template match="item[@topic]"/>
  <xsl:template match="Done">
    <h2><a name="Done">Done</a>  (<xsl:value-of select="count(items/item)"/>)</h2>
    <xsl:call-template name="make-topic-list">
      <xsl:with-param name="nodes" select="items/item"/>
      <xsl:with-param name="topic" select="Done"/>
    </xsl:call-template>
  </xsl:template>
  <!--          use="concat(parent::items/parent::topic/title, @status)" -->
  <xsl:key name="status" match="item[@status]" use="concat(@topic, @status)"/>
  <xsl:template match="topic">
    <xsl:call-template name="make-topic-list">
      <xsl:with-param name="nodes" select="items/item[not(@topic)]"/>
      <xsl:with-param name="topic" select="./title"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="make-topic-list">
    <xsl:param name="nodes"/>
    <xsl:param name="topic"/>
    <h2 class="topic">
      <a>
        <xsl:attribute name="name">
          <xsl:value-of select="$topic"/>
        </xsl:attribute>
        <xsl:value-of select="$topic"/>
      </a>
    </h2>
    <xsl:if test="count($nodes) &gt; 0">
      <xsl:call-template name="topicCountTable">
        <xsl:with-param name="nodes" select="$nodes"/>
      </xsl:call-template>
    </xsl:if>
    <ol>
      <dl>
        <!-- Notify status -->
        <xsl:for-each select="$nodes[@status='notify']">
          <xsl:call-template name="item"/>
        </xsl:for-each>
        <!-- Question status -->
        <xsl:for-each select="$nodes[@status='q']">
          <xsl:call-template name="item"/>
        </xsl:for-each>
        <!-- High status -->
        <xsl:for-each select="$nodes[@status='high']">
          <xsl:call-template name="item"/>
        </xsl:for-each>
        <!-- No status -->
        <xsl:for-each select="$nodes[not(@status)]">
          <xsl:call-template name="item"/>
        </xsl:for-each>
        <!-- Any with a status attribute. Need to pick up only the later at present.-->
        <!--  <xsl:for-each select="items/item[@status='later']"> -->
        <xsl:for-each select="$nodes[not(not(@status) or @status='done' or @status='high' or @status = 'q' or @status = 'notify')]">
          <xsl:call-template name="item"/>
        </xsl:for-each>
        <hr width="50%" height="2"/>
        <!-- Done status. -->
      <xsl:if test="$show.done">
        <div class="done">
         <div class="visible">
          <xsl:for-each select="$nodes[@status='done']">
            <xsl:call-template name="item"/>
          </xsl:for-each>
	 </div>
        </div>
      </xsl:if>
      </dl>
    </ol>
  </xsl:template>
  <xsl:template match="xref">
    <a>
      <xsl:attribute name="href">#<xsl:value-of select="@linkend"/></xsl:attribute>
      <xsl:apply-templates/>
    </a>
  </xsl:template>
  <xsl:template name="item" match="item/item">
    <div class="todoItem">
     <div><xsl:if test="./item"><xsl:attribute name="class">nestedItems</xsl:attribute></xsl:if>
      <xsl:element name="dt">
        <xsl:attribute name="class">
          <xsl:value-of select="@status"/>
        </xsl:attribute>
        <!--   [<xsl:value-of select="position()"/>]    -->
        <li>
          <xsl:attribute name="class">
            <xsl:value-of select="@status"/>
          </xsl:attribute>
          <xsl:attribute name="title">
            <xsl:value-of select="@status"/>
          </xsl:attribute>
          <a>
            <xsl:attribute name="name">
              <xsl:value-of select="concat(@topic, '::', @status)"/>
            </xsl:attribute>
          </a>
          <xsl:if test="@id">
            <a>
              <xsl:attribute name="name">
                <xsl:value-of select="@id"/>
              </xsl:attribute>
            </a>
          </xsl:if>
          <xsl:if test="@status = 'confirm'">
            <b class="confirm">Ok?</b>
          </xsl:if>
          <xsl:if test="not(@status = 'done')">
            <b class="status">
              <xsl:value-of select="@status"/>
            </b>
          </xsl:if>
          <xsl:if test="@status = 'partial'">
            <img src="inst/Icons/ConstructionIcon.jpg" width="32" height="32"/>
          </xsl:if>
          <xsl:apply-templates select="*|solution|br|p|para|issue|pre|text()|xref|check|r:output|r:code|r:func|r:class|reply|q|item"/>
        </li>
      </xsl:element>
      <xsl:if test="not(@status='done')">
        <xsl:element name="dd">
          <xsl:attribute name="class">
            <xsl:value-of select="@status"/>
          </xsl:attribute>
          <xsl:apply-templates select="comment"/>
        </xsl:element>
      </xsl:if>
     </div>
    </div>
  </xsl:template>
  <xsl:template match="item[@status='check' or @status='partial']">
    <p>
      <xsl:attribute name="class">
        <xsl:value-of select="@status"/>
      </xsl:attribute>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <!-- Show the counts for a given topic -->
  <xsl:template name="topicCountTable">
    <xsl:param name="nodes" select="./items/item[not(@topic)]"/>
    <xsl:param name="topic"/>
    <div class="topic-table">
      <p align="center">
        <a>
          <xsl:attribute name="href">#<xsl:value-of select="$topic"/></xsl:attribute>
          <xsl:value-of select="$topic"/>
        </a>
      </p>
      <table width="200">
        <tbody>
          <tr>
            <th>
              <i>Status</i>
            </th>
            <th>
              <i>Count</i>
            </th>
          </tr>
          <!-- XXX this is dropping elements in the @topic world. -->
          <xsl:variable name="status-list" select="//item[not(@status=preceding::item/@status)]/@status"/>
          <xsl:for-each select="$status-list">
            <xsl:variable name="status-label" select="."/>
            <xsl:if test="count($nodes[@status=$status-label])">
              <tr>
                <td align="left">
                  <xsl:attribute name="class">
                    <xsl:value-of select="string(.)"/>
                  </xsl:attribute>
                  <a>
                    <xsl:attribute name="class">
                      <xsl:value-of select="string(.)"/>
                    </xsl:attribute>
                    <xsl:attribute name="href">#<xsl:value-of select="$topic"/>::<xsl:value-of select="$status-label"/></xsl:attribute>
                    <xsl:value-of select="string(.)"/>
                  </a>
                </td>
                <td align="center">
                  <xsl:attribute name="class">
                    <xsl:value-of select="string(.)"/>
                  </xsl:attribute>
                  <xsl:value-of select="count($nodes[@status=$status-label])"/>
                </td>
              </tr>
            </xsl:if>
          </xsl:for-each>
          <xsl:if test="count($nodes[not(@status)]) &gt; 0">
            <tr>
              <td align="left" class="no-status">
                <a class="no-status"><xsl:attribute name="href">#<xsl:value-of select="$topic"/></xsl:attribute>no status</a>
              </td>
              <td align="center">
                <xsl:value-of select="count($nodes[not(@status)])"/>
              </td>
            </tr>
          </xsl:if>
          <tr>
            <th/>
            <th>
              <hr width="50%"/>
            </th>
          </tr>
          <tr>
            <th/>
            <td align="center">     (<xsl:value-of select="count($nodes)"/>)</td>
          </tr>
        </tbody>
      </table>
    </div>
  </xsl:template>
  <!-- ******************************************************************************************************** -->
  <xsl:template match="p">
    <br/>
    <!-- <xsl:copy select="@*"/> -->
    <xsl:apply-templates/>
    <br/>
  </xsl:template>
  <xsl:template match="p[@class]">
    <br/>
    <p>
      <xsl:copy-of select="@*"/>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xsl:template match="br">
    <br/>
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="linerule">
    <center>
      <hr width="33%"/>
    </center>
  </xsl:template>
  <xsl:template match="info">
    <i class="info">
      <xsl:apply-templates/>
    </i>
  </xsl:template>
  <xsl:template match="em">
    <em class="emphasis">
      <xsl:apply-templates/>
    </em>
  </xsl:template>
  <!-- Copy regular HTML tags through. -->
  <xsl:template match="i|b|ul|li|hr|sub|sup|i|object|param|table|tr|td|th|h1|h2|h3|h4|h5|h6|code|form|font|html|head|body|OBJECT|PARAM|br|option|select|optgroup|input|pre|ol">
    <xsl:element name="{name(.)}">
      <xsl:for-each select="@*">
        <xsl:copy/>
      </xsl:for-each>
      <xsl:apply-templates/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="ulink">
    <a href="{@url}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>
  <xsl:template match="pre[@popup='true']">
    <a>
      <xsl:attribute name="id">
        <xsl:value-of select="@id"/>
      </xsl:attribute>
      <xsl:attribute name="onMouseOver">
    ShowTip(event, '<xsl:value-of select="@id"/>-pop', '<xsl:value-of select="@id"/>')
   </xsl:attribute>
      <xsl:attribute name="onMouseOut">
    HideTip('<xsl:value-of select="@id"/>-pop')
   </xsl:attribute>
      <code class="popup">popup</code>
    </a>
    <div class="CToolTip">
      <xsl:attribute name="id"><xsl:value-of select="@id"/>-pop</xsl:attribute>
      <pre>
        <xsl:apply-templates/>
      </pre>
    </div>
  </xsl:template>
  <xsl:template match="r:code">
    <pre class="rcode">
      <xsl:apply-templates/>
    </pre>
  </xsl:template>
  <xsl:template match="r:output">
    <pre class="routput">
      <xsl:apply-templates/>
    </pre>
  </xsl:template>
  <xsl:template match="r:func">
    <code class="rfunc"><xsl:apply-templates/>()</code>
  </xsl:template>
  <xsl:template match="r:class">
    <code class="rclass">
      <xsl:apply-templates/>
    </code>
  </xsl:template>
  <xsl:template match="reply">
    <br/>
    <div class="reply">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xsl:template match="q">
    <xsl:message>Processing q node</xsl:message>
    <p class="question">
      <b class="questionMarker">???:</b>
      <xsl:apply-templates/>
    </p>
  </xsl:template>
  <xsl:template match="solution">
    <xsl:message>solution</xsl:message>
    <div class="solution">
      <xsl:apply-templates/>
    </div>
  </xsl:template>
  <xsl:template match="xref">
    <a><xsl:attribute name="href">#<xsl:value-of select="@linkend"/></xsl:attribute>here</a>
  </xsl:template>

  <xsl:template match="google|searchQ">Search Query: <div class="searchQuery"><xsl:apply-templates/></div></xsl:template>
</xsl:stylesheet>
 

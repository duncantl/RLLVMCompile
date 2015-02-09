<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
<html>
  <head>
    <title>Todo List <xsl:apply-templates select="/topics/title"/></title>
    <link rel="stylesheet" href="http://www.omegahat.org/Todo.css" />
    <link rel="stylesheet" href="../../Docs/CSS/Todo.css" />
  </head>
  <body>
    <ol>
     <xsl:for-each select="/topics/topic">
      <li>
       <xsl:element name="a">
         <xsl:attribute name="href">#<xsl:value-of select="title"/></xsl:attribute>
           <xsl:value-of select="title"/>
       </xsl:element>
       - <xsl:value-of select="count(./items/item)"/>
      </li>
     </xsl:for-each>
       <li><a href="#Done">Done</a></li>
    </ol>

   Total number of elements: <xsl:value-of select="count(//item)"/>
<br/>
   Not yet completed: <xsl:value-of select="count(//item[not(@status='done') and not(@status='info')])"/>
<br/>
   High priority: <xsl:value-of select="count(//item[@status='high' or not(@status)])"/>
<br/>
   Completed: <xsl:value-of select="count(//item[@status='done' or @status='info'])"/>
    <p/>
     <hrule width="50%"/>
     
      <xsl:call-template name="showCountTables" />

    <p/>
     <hrule width="50%"/>

   <xsl:apply-templates />

  </body>
</html>
</xsl:template>

<xsl:template match="/topics/title" />

<xsl:template name="showCountTables">
  <table>
   <tr valign="top">
    <xsl:for-each select="topics/topic">
      <th>
       <table> 
	<tbody>
          <tr><th><xsl:element name="a"><xsl:attribute name="href">#<xsl:value-of select="title"/></xsl:attribute><xsl:value-of select="title"/></xsl:element></th></tr>
          <tr><th><xsl:call-template name="topicCountTable"/></th></tr>
	</tbody>
	</table>
      </th>
    </xsl:for-each>
   </tr>
  </table>
</xsl:template>



<xsl:template match="em">
 <em class="emphasis">
  <xsl:apply-templates />
 </em>
</xsl:template>

<!--
  Need to find a way to use a different match="item" template.

<xsl:key name="TotalStatus" match="items/item[@status]" use="@status" />

<xsl:template name="totalCountTable">
</xsl:template>
-->
<!-- Hint from http://www.dpawson.co.uk/xsl/sect2/N2018.html#d4e412 -->


<xsl:template match="item">
  <tr>
    <xsl:element name="td">
    <xsl:attribute name="align">left</xsl:attribute>
    <xsl:attribute name="class"><xsl:value-of select="@status"/></xsl:attribute>
        <xsl:value-of select="@status" />
    </xsl:element>
    <td align="right"><xsl:value-of select="count(key('status', concat(../../title, @status)))" /></td>
  </tr>
</xsl:template>


<xsl:template match="Done">
 <h2><a name="Done">Done</a>  (<xsl:value-of select="count(items/item)"/>)</h2>
   <xsl:call-template name="topicList"/>
</xsl:template>


<xsl:key name="status" match="items/item[@status]"
          use="concat(parent::items/parent::topic/title, @status)" />

<xsl:template match="topic">

<h2>
       <xsl:element name="a">
         <xsl:attribute name="name"><xsl:value-of select="title"/></xsl:attribute>
           <xsl:value-of select="title" />
       </xsl:element>
     <br/>
</h2>

  <xsl:call-template name="topicCountTable"/>
  <xsl:call-template name="topicList"/>

</xsl:template>


<xsl:template name="topicList">
<ol>
<dl>
  <!-- High status -->
 <xsl:for-each select="items/item[@status='high']">
   <xsl:call-template name="item"/>
 </xsl:for-each>
 <!-- No status -->
 <xsl:for-each select="items/item[not(@status)]">
   <xsl:call-template name="item"/>
 </xsl:for-each>
 <!-- Any with a status attribute. 
        Need to pick up only the later at present.-->
<!--  <xsl:for-each select="items/item[@status='later']"> -->
  <xsl:for-each select="items/item[not(not(@status) or @status='done' or @status='high')]"> 
   <xsl:call-template name="item"/>
 </xsl:for-each>
 <!-- Done status. -->
 <xsl:for-each select="items/item[@status='done']">
   <xsl:call-template name="item"/>
 </xsl:for-each>
</dl>
</ol>
</xsl:template>


<xsl:template name="item">
  <div class="todoItem">
   <xsl:element name="dt">
     <xsl:attribute name="class"><xsl:value-of select="@status"/></xsl:attribute>
    <!--   [<xsl:value-of select="position()"/>]    -->
     <li><xsl:apply-templates select="issue" /></li>   
   </xsl:element>

   <xsl:if test="not(@status='done')">
    <xsl:element name="dd">
     <xsl:attribute name="class"><xsl:value-of select="@status"/></xsl:attribute>
     <xsl:apply-templates select="comment"/>
    </xsl:element>
   </xsl:if>
  </div>
</xsl:template>



<xsl:template name="topicCountTable">
     <table width="200">
       <tbody>
<!--
  Using the concatenation, we get within topic counts.
  If we omitted the concatenation, we would get counts across the
  entire document!

  See http://www.biglist.com/lists/xsl-list/archives/200011/msg00210.html
  for an example.
-->
        <tr><th><i>Status</i></th><th><i>Count</i></th></tr>
        <xsl:apply-templates
            select="./items/item[@status and 
                                 generate-id(.)=generate-id(key('status',
                                   concat(../../title, @status)))]" />

        <tr>
          <td align="left">no status</td>
          <td align="right"><xsl:value-of select="count(./items/item[not(@status)])"/> </td>
        </tr>
        <tr><th></th><td align="right">     (<xsl:value-of select="count(./items/item)" />)</td></tr>
      </tbody>
     </table>

</xsl:template>




<xsl:template match="br">
   <br/>
     <xsl:apply-templates/>
</xsl:template>


<xsl:template match="linerule">
  <center><hr width="33%"/></center>
</xsl:template>


<xsl:template match="info">
  <i class="info"><xsl:apply-templates /></i>
</xsl:template>




<!-- Copy regular HTML tags through. -->
<xsl:template match="ul|li|hr|sub|sup|i|object|param|table|tr|td|th|h1|h2|h3|h4|h5|h6|code|form|font|html|head|body|OBJECT|PARAM|br|option|select|optgroup|input|pre|ol">
 <xsl:element name="{name(.)}">
  <xsl:for-each select="@*">
   <xsl:copy />
  </xsl:for-each>
   <xsl:apply-templates />
 </xsl:element>
</xsl:template>



</xsl:stylesheet>

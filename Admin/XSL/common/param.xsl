<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:param name="with-exercises" select="0"/>
<xsl:param name="with-exercise-answers" select="0"/>
<xsl:param name="case-studies-image-dir" select="."/>

<xsl:param name="case-studies-icon-dir" select="'../Admin/icons'" />
<xsl:param name="image-extension" select="'png'" />

<xsl:param name="for-instructors" select="0"/>




<xsl:template match="statPrereqs">
<xsl:call-template name="topicList">
<xsl:with-param name="title">Statistics Prerequisites</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xsl:template match="computingPrereqs">
<xsl:call-template name="topicList">
 <xsl:with-param name="title">Computing Prerequisites</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xsl:template match="statTopics">
<xsl:call-template name="topicList">
 <xsl:with-param name="title">Statistics Topics Covered</xsl:with-param>
</xsl:call-template>
</xsl:template>

<xsl:template match="computingTopics">
<xsl:call-template name="topicList">
 <xsl:with-param name="title">Computing Topics Covered</xsl:with-param>
</xsl:call-template>
</xsl:template>


<xsl:template match="statPrereqs/topic|computingPrereqs/topic|computingTopics/topic|statTopics/topic">
<xsl:apply-templates/>
</xsl:template>

</xsl:stylesheet>

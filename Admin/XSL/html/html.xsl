<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
                xmlns:html="http://www.w3.org/TR/html401"
                version="1.0">

<xsl:template match="html:tag">
<font class="htmlTag"><xsl:apply-templates /></font>
</xsl:template>

<xsl:template match="html:attribute|html:attr">
 <font class="htmlAttribute"><xsl:apply-templates /></font>
</xsl:template>

</xsl:stylesheet>

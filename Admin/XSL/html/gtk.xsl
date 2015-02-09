<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:gtk="http://www.gtk.org"
                version="1.0">


<xsl:template match="gtk:widget">
 <i><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="gtk:signal">
 <code>"<xsl:apply-templates/>"</code>
</xsl:template>

</xsl:stylesheet>
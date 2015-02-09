<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:r="http://www.r-project.org"
		xmlns:s="http://cm.bell-labs.com/stat/S4"
		xmlns:c="http://www.C.org"
                xmlns:python="http://www.python.org"
                xmlns:perl="http://www.perl.org"
		xmlns:vb="http://www.visualbasic.com"
		xmlns:omegahat="http://www.omegahat.org"
                xmlns:bioc="http://www.bioconductor.org"
                xmlns:java="http://www.java.com"
		xmlns:statdocs="http://www.statdocs.org"
		xmlns:gtk="http://www.gtk.org"
		xmlns:com="http://www.microsoft.com"
		xmlns:rx="http://www.regex.org"
                version="1.0">


<xsl:template match="rx:pattern|rx:expr">
  <code class="regexpattern"><xsl:apply-templates /></code>
</xsl:template>



</xsl:stylesheet>

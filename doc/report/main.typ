#import "mastery-chs/lib.typ" : template, appendices

//#set text(font: "DejaVu Sans")

#let department = "Department of Computer Science and Engineering"
#show: template.with(
  title: "Compiled e-graphs with primitives",
  subtitle: "Some e-graphy subtitle",
  authors: ("Loke Gustafsson", "Erik Magnusson"),
  department: department,
  supervisor: ("Hazem Torfah", department),
  advisor: ("Alejandro Luque Cerpa", department),
  examiner: ("Matti Karppa", department),
  abstract: [Abstract text about your project in Computer Science and Engineering],
  keywords: ("e-graphs", "equality saturation", "datalog", "program optimization", "rewrite rules"),
  acknowledgements: [Here, you can say thank you to your supervisor(s), company advisors and other
  people that supported you during your project.],
)

= Introduction

A reference @egglog.

This is referencing a section: @thesection.

And this is referencing the appendix: @theappendix.

== A subsection

=== A subsubsection

==== A subsubsection

= Background <thesection>

= Name of our thing

= Benchmarks

= Conclusion

#bibliography("refs.bib")
#counter(heading).update(0)
#set heading(numbering: "A.1", supplement: [Appendix])

= Some appendix here <theappendix>

== wow

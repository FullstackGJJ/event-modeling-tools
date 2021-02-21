Resource for compiling tools necessary to make event modeling as good as it can be

If you want to read up on what event modeling is read this [article](https://eventmodeling.org/posts/what-is-event-modeling/)

Currently, event modeling is primarily used to describe user and system interactions. It is a great tool for requirement gathering and domain modeling. I see great potential in test specifications, scenario previewing, and snapshot views of different scope of the project and systems. I think that event modeling is a very intuitive way to think about design and has the potential to clarify even the structure of code in a project. I think we can put it to the test as an alternative form of documentation (good documentation can be hard to come by). I even want to try out showing an event model as a form of demonstrating setup instruction or bug reproducer.

The diagram shown below is a fairly thorough template of what event modeling would look like. I might even play around with making it vertical instead of horizontal.

![alt text](event_modeling_diagram.jpg "Event Modeling Diagram template")

That style of left to right representation of events unfolding horizontally in a timeline make sense as if you're watching a movie with sliding frame and animation. Would be even better if you have a sliding frame with an electron lighting up to indicate how time is moving forward and what is happening at a given time. 

What if you don't have a sliding frame? Imagine you have a long workflow/process that calls upon many many things, does it make sense to have a diagram that goes from left to right to infinity? Not really, if you're looking at a static frame, it might make more sense to read chart or text from top to bottom. What kind of physical medium would that look like in current day (02/21/2021)? I suspect that a comic book or old fashion 8mm film still frames would be a good visual representation that goes from top to bottom, and a stage play would be a good text representation for how events unfold in a format that goes from top to bottom (referencing the stage_play_sample.pdf as an example).

A Converter Story

Cast of Actors:
Users: 
  - Designers: People who design document templates
Systems:
  - Reporting: System that renders DocumentTemplates into DocumentPdfs
  - Converter: System that converts OldDocumentDataFormat reference to NewDocumentDataFormat in DocumentTemplates
  - Repository: System that stores document DocumentTemplates
  - Data Provider: System that provides data relevant for DocumentTemplates
  - Validator: System that validates if DocumentTemplates produce the the same pdf
Data:
  - DocumentTemplate
  - DocumentPdf
  - OldDocumentDataFormat
  - NewDocumentDataFormat
  - SubstitutionRule

Setting: We are in the office where the underlying data format provided by Data Provider has changed, thereby changing how the DocumentTemplate needs to define its DocumentData. We want a system that can convert all existing DocumentTemplates' reference of OldDocumentDataFormat the NewDocumentDataFormat that the Data Provider is going to move to and also ensure that the DocumentPdf that get rendered is still correct. 

Scenario 1: A successful conversion

Designer: Converter, convert DocumentTemplate:"Awesome.json" from OldDocumentDataFormat:"mass effect 1" to NewDocumentDataFormat:"mass effect 2"
Converter: (convert request recieved from Designer)
		   Data Provider, getSubstitutionRule for OldDocumentDataFormat:"mass effect 1" to NewDocumentDataFormat:"mass effect 2"
Data Provider: (getSubstitutionRule request received from Converter)
			   Converter, getSubstitutionRule response is SubstitutionRule:"mass effect rule"
Converter: (getSubstitutionRule response SubstitutionRule:"mass effect rule" received from Data Provider)
		   (apply SubstitutionRule:"mass effect rule" to DocumentTemplate:"Awesome.json")
		   Reporting, render DocumentTemplate:"Awesome.json"
Reporting: (render request received from Converter)
		   Converter, render response is DocumentPdf:"Awesome.pdf"
Converter: (render response DocumentPdf:"Awesome.pdf" received from Reporting)
		   Validator, compare DocumentPdf:"Awesome.pdf" to reference DocumentPdf:"Awesome.pdf"
Validator: (compare request received from Converter)
		   Converter, compare response is "Success"
Converter: (compare response "Success" received from Validator)
		   Repository, store new DocumentTemplate:"Awesome.json" over existing one
Repository: (store request received from Converter)
			Converter, store response is "Success"
Converter: (store response "Success" received from Repository)
		   Designer, convert response is "Success"
Designer: (convert response received from Converter)

# Event Play Script Domain

**include Script domain**

**data** Name

**data** Description

**data** ActorProfile = ActorProfile Name Description

**data** User: ActorProfile

**data** Users: [ User ]

**data** System: ActorProfile

**data** Systems: [ System ]

**data** DataType: DataType Name Description

**data** DataTypes: [ DataType ]

**data** Setting

**data** Scope

**data** Scenario

**data** EventPlayScript = EventPlayScript Users Systems DataTypes Setting Scope Scenario Script

**data** FilterParameter

_filter :: EventPlayScript -> FilterParameter -> EventPlayScript_

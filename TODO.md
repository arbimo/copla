TODO
====

Model:

- Any Elem using a part of the name space should extends Declaration(id: Id)
- anmlParse should use End~PathWith(model)
- refactor filter(isDefined(_)).map(_.get) as flatMap { case Some(x) => PassWIth(x) case None => Fail }
- in word parser, use noApi.~ instead of importing all._ locally

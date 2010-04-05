function (newDoc, oldDoc, userCtx) {

    function require (field, constr) {
	if (!newDoc [field])
	    throw ({forbidden : "document must have a " + field + " field"});
	if (newDoc [field].constructor.name != constr.name) {
	    log (newDoc [field]);
	    log (newDoc [field].constructor.name);
	    log (constr.name);
	    throw ({forbidden : "field " + field + " has the wrong type"});
	}
    };

    log (userCtx);

    if (!userCtx.name) {
	throw ({forbidden : 'only registered users are allowed to add or edit filters'});
    }

    if (userCtx.roles.indexOf ('_admin') != -1) {
	return;
    }

    if (oldDoc) {
	var oldmatch = /^org\.mathmap\.([_a-zA-Z][_0-9a-zA-Z]*)\.([_a-zA-Z][_0-9a-zA-Z]*)$/.exec (oldDoc._id);
	log (oldmatch);

	if (!oldmatch || oldmatch [1] != userCtx.name) {
	    throw ({forbidden : 'cannot add or edit another user\'s filter'});
	}
    }

    var newmatch = /^org\.mathmap\.([_a-zA-Z][_0-9a-zA-Z]*)\.([_a-zA-Z][_0-9a-zA-Z]*)$/.exec (newDoc._id);
    log (newmatch);
    if (!newmatch) {
	throw ({forbidden : '_id is not a well-formed MathMap filter name'});
    }

    if (newmatch [1] != userCtx.name) {
	throw ({forbidden : 'cannot add or edit another user\'s filter'});
    }

    require ("content", String);
    require ("kind", String);
    require ("tags", Array);
    require ("title", String);

    for (var i in newDoc.tags) {
	if (newDoc.tags [i].constructor != String) {
	    throw ({forbidden : "tags must be strings"});
	}
    }
}

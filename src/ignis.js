function Ignis (url, connection) {
    this.ignis = this;
    if (url) {
        var parsed = Ignis.parseURL(url);
        this.shared = { url: parsed.protocol + "//" + parsed.host };
        this.shared.state = Haste.ignis(this.shared,
                                        connection
                                        ? connection
                                        : new Ignis.Connection(parsed.host));
        
        Ignis.buildPath(null, this, parsed.path, function () {
            return new Ignis(null);
        });
    }
}

(function() {
    Ignis.ServerValue = new Object();
    Ignis.ServerValue.TIMESTAMP = new Object();

    function Connection(host) {
        this.host = host;
    }

    Ignis.Connection = Connection;

    function parsePath (path, start) {
        var split = path.split('/');
        var parent = null;
        var path = start;
        for (var i = 0; i < split.length; ++i) {
            var s = split[i];
            if (s.length !== 0) {
                var n = { key: s,
                          path: Haste.append(path, s),
                          parent: parent };
                parent = n;
                path = n.path;
            }
        }
        return parent;
    }

    function skip(toSkip, s) {
        while (s.indexOf(toSkip) == 0) {
            s = s.substring(toSkip.length);
        }
        return s;
    }
    
    Ignis.parseURL = function (url) {
        var index = url.indexOf(":");
        if (index > 0) {
            var result = { protocol: url.substring(0, index + 1) }
            url = skip("/", url.substring(index + 1));
            
            index = url.indexOf("/");
            if (index > 0) {
                result.host = url.substring(0, index);
                result.path = url.substring(index + 1);
            } else {
                result.host = url;
                result.path = "";
            }

            return result;
        } else {
            throw new Error("malformed URL: " + url);
        }
    }
    
    Ignis.buildPath = function (parent, child, path, make) {
        var p = parsePath(path, parent ? parent.path : null);
        var n = child;
        while (p) {
            n.key = p.key;
            n.path = p.path;
            if (p.parent) {
                n.parent = make();
                n.parent.shared = shared;
                n = n.parent;
            }
            p = p.parent;
        }
        n.parent = parent;
        return child;
    }
    
    function DataSnapshot (ignis, state, path, key) {
        // todo: rather than track the state and path independently,
        // we can make state point directly to the location of
        // interest since the snapshot is immutable
        this.ignis = ignis;
        this.state = state;
        this.path = path;
        this.key = key;
    }

    DataSnapshot.prototype.exists = function () {
        return this.val() !== null;
    }

    DataSnapshot.prototype.val = function () {
        return Haste.value(this.state, this.path);
    }

    DataSnapshot.prototype.child = function (path) {
        return (function (ignis, state, parent) {
            return Ignis.buildPath
            (parent, new DataSnapshot(ignis, state, null, null), path,
             function() {
                 return new DataSnapshot(ignis, state, null, null);
             });
        })(this.ignis, this.state, this);
    }

    DataSnapshot.prototype.forEach = function (onChild) {
        return Haste.forEach(this.state, this.path, onChild);
    }

    DataSnapshot.prototype.hasChild = function (path) {
        return this.child(path).exists();
    }

    DataSnapshot.prototype.hasChildren = function () {
        return Haste.hasChildren(this.state, this.path);
    }

    DataSnapshot.prototype.numChildren = function () {
        return Haste.countChildren(this.state, this.path);
    }

    DataSnapshot.prototype.key = function () {
        return this.key;
    }

    DataSnapshot.prototype.ref = function () {
        return this.ignis;
    }

    DataSnapshot.prototype.getPriority = function () {
        return Haste.priority(this.state, this.path);
    }

    DataSnapshot.prototype.exportVal = function () {
        return Haste.exportValue(this.state, this.path);
    }
    
    Ignis.DataSnapshot = DataSnapshot;
    
    Ignis.prototype.authWithPassword =
        function (credentials, onComplete, options) {
            Haste.authenticate(this.shared.state, credentials, onComplete,
                               options);
        }
    
    Ignis.prototype.authAnonymously = function (onComplete, options) {
        this.authWithPassword(null, oncomplete, options);
    }

    Ignis.prototype.getAuth = function () {
        return Haste.getAuth(this.shared.state);
    }

    Ignis.prototype.onAuth = function (onComplete, context) {
        Haste.onAuth(this.shared.state, onComplete, context, onComplete);
    }

    Ignis.prototype.offAuth = function (onComplete, context) {
        Haste.offAuth(this.shared.state, onComplete, context);
    }

    Ignis.prototype.unauth = function () {
        Haste.unauth(this.shared.state);
    }

    Ignis.prototype.child = function (path) {
        return (function (shared, parent) {
            var child = new Ignis(null);
            child.shared = shared;

            return Ignis.buildPath
            (this, child, path, function() {
                var n = new Ignis(null);
                n.shared = shared;
                
                return n;
            });
        })(this.shared, this);
    }

    Ignis.prototype.parent = function () {
        return this.parent;
    }

    Ignis.prototype.root = function () {
        var n = this;
        while (n.parent) {
            n = n.parent;
        }
        return n;
    }

    Ignis.prototype.key = function () {
        return this.key;
    }

    Ignis.prototype.toString = function () {
        if (this.parent) {
            return this.parent.toString() + "/" + this.key;
        } else {
            return this.shared.url;
        }
    }

    Ignis.prototype.set = function (value, onComplete) {
        return this.setWithPriority(value, null, onComplete);
    }

    Ignis.prototype.update = function (value, onComplete) {
        return Haste.update(this.shared.state, this.path, value, onComplete);
    }

    Ignis.prototype.remove = function (onComplete) {
        this.set(null, onComplete);
    }

    Ignis.prototype.push = function (value, onComplete) {
        var child = this.child(Haste.makeName(this.shared.state));
        if (value) {
            child.set(value, onComplete);
        }
    }

    Ignis.prototype.setWithPriority = function (value, priority, onComplete) {
        return Haste.set(this.shared.state, this.path, value, priority,
                         onComplete);
    }

    Ignis.prototype.setPriority = function (priority, onComplete) {
        return Haste.setPriority(this.shared.state, this.path, priority,
                                 onComplete);
    }

    Ignis.prototype.transaction = function (update, onComplete, applyLocally) {
        return Haste.setAtomic(this.shared.state, this.path, update,
                               onComplete, applyLocally);
    }

    Ignis.prototype.goOnline = function () {
        Haste.goOnline(this.shared.state);
    }

    Ignis.prototype.goOffline = function () {
        Haste.goOffline(this.shared.state);
    }

    function Query(shared, ignis, path, query) {
        this.shared = shared;
        this.ignis = ignis;
        this.path = path;
        this.query = query;
    }
    
    Query.prototype.on = Ignis.prototype.on =
        function (eventType, onEvent, onCancel, context) {
            Haste.on(this.shared.state, this.path, this.query, eventType,
                     onEvent, onCancel, context, this);
            
            return onEvent;
        }

    Query.prototype.off = Ignis.prototype.off =
        function (eventType, onEvent, context) {
            Haste.off(this.shared.state, this.path, this.query, eventType,
                      onEvent, context);
        }

    Query.prototype.once = Ignis.prototype.once =
        function (eventType, onEvent, onCancel, context) {
            Haste.once(this.shared.state, this.path, this.query, eventType,
                       onEvent, context);
        }

    Query.prototype.orderByChild = Ignis.prototype.orderByChild =
        function (key) {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query, "orderByChild"));
        }

    Query.prototype.orderByKey = Ignis.prototype.orderByKey = function () {
        return new Query(this.shared,
                         this.ignis,
                         this.path,
                         Haste.appendQuery(this.query, "orderByKey"));
    }

    Query.prototype.orderByValue = Ignis.prototype.orderByValue = function () {
        return new Query(this.shared,
                         this.ignis,
                         this.path,
                         Haste.appendQuery(this.query, "orderByValue"));
    }

    Query.prototype.orderByPriority = Ignis.prototype.orderByPriority =
        function () {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query, "orderByPriority"));
        }

    Query.prototype.startAt = Ignis.prototype.startAt =
        function (value, key) {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query,
                                               "startAt",
                                               value,
                                               key));
        }

    Query.prototype.endAt = Ignis.prototype.endAt =
        function (value, key) {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query,
                                               "endAt",
                                               value,
                                               key));
        }

    Query.prototype.equalTo = Ignis.prototype.equalTo =
        function (value, key) {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query,
                                               "equalTo",
                                               value,
                                               key));
        }

    Query.prototype.limitToLast = Ignis.prototype.limitToLast =
        function (count) {
            return new Query(this.shared,
                             this.ignis,
                             this.path,
                             Haste.appendQuery(this.query,
                                               "limitToLast",
                                               count));
        }

    Query.prototype.ref = Ignis.prototype.ref = function() {
        return ignis;
    }

    function OnDisconnect(shared, path) {
        this.shared = shared;
        this.path = path;
    }

    Ignis.prototype.onDisconnect = function() {
        return new OnDisconnect(this.shared, this.path);
    }

    OnDisconnect.prototype.set = function (value, onComplete) {
        return this.setWithPriority(value, null, onComplete);
    }

    OnDisconnect.prototype.update = function (value, onComplete) {
        return Haste.updateOnDisconnect(this.shared.state, this.path, value,
                                        onComplete);
    }

    OnDisconnect.prototype.remove = function (onComplete) {
        return this.set(null, onComplete);
    }

    OnDisconnect.prototype.setWithPriority =
        function (value, priority, onComplete) {
            return Haste.setOnDisconnect(this.shared.state, this.path, value,
                                         priority, onComplete);
        }

    OnDisconnect.prototype.cancel = function (onComplete) {
        return Haste.cancelOnDisconnect(this.shared.state, this.path,
                                        onComplete);
    }
})();

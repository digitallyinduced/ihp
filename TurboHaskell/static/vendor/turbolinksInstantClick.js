// based on https://gist.github.com/Ch4s3/49a018d32e93bbf7d52064ad9b314d78
const OldHttpRequest = Turbolinks.HttpRequest;

Turbolinks.CachedHttpRequest = class CachedHttpRequest extends Turbolinks.HttpRequest {
  constructor(_, location, referrer) {
    super(_, location, referrer);
    this.delegate = this;
  }

  requestCompletedWithResponse(response, redirectedToLocation) {
    this.response = response;
    return(this.redirect = redirectedToLocation);
  }

  requestFailedWithStatusCode(code) {
    return(this.failCode = code);
  }

  oldSend() {
    if (this.xhr && !this.sent) {
      this.notifyApplicationBeforeRequestStart();
      this.setProgress(0);
      this.xhr.send();
      this.sent = true;
      return(this.delegate, 'requestStarted', o => o.requestStarted());
    }
  }

  send() {
    if (this.failCode) {
      return(this.delegate.requestFailedWithStatusCode(this.failCode, this.failText));
    } else if (this.response) {
      return(this.delegate.requestCompletedWithResponse(this.response, this.redirect));
    } else {
      return(this.oldSend());
    }
  }
};


Turbolinks.HttpRequest = class HttpRequest {
  constructor(delegate, location, referrer) {
    const cache = Turbolinks.controller.cache.get(`prefetch${location}`);
    if (cache) {
      Turbolinks.controller.cache.delete(`prefetch${location}`);
      cache.delegate = delegate;
      return cache;
    } else {
      return(new OldHttpRequest(delegate, location, referrer));
    }
  }
};

Turbolinks.SnapshotCache.prototype.delete = function(location) {
  const key = Turbolinks.Location.wrap(location).toCacheKey();
  return(delete this.snapshots[key]);
};

const preloadAttribute = function(link) {
return true;
  const linkAttr = link.attributes['data-turbolinks-preload']
  if (!linkAttr || linkAttr.value === 'false') {
    return false;
  } else {
    return true;
  }
}

const isNotGetMethod = function(link) {
  return link.classList.contains('js-delete');
}

//This function returns true if the link or location shouldn't be
const notPreloadable = function(link, location){
  if (preloadAttribute(link) === false) {
    return true;
  } else if (isNotGetMethod(link)) {
    return true;
  } else if (location.anchor || location.absoluteURL.endsWith("#")) {
    return true;
  } else if (location.absoluteURL === window.location.href) {
    return true;
  } else {
    return false;
  }
}

const preload = function(event) {
  let link = Turbolinks.controller.getVisitableLinkForNode(event.target);
  if (link) {
    let location = Turbolinks.controller.getVisitableLocationForLink(link);
    if (location) {
      if (Turbolinks.controller.applicationAllowsFollowingLinkToLocation(link, location)) {
        if (notPreloadable(link, location)) {
          return;
        }
        // If Turbolinks has already cached this location internally, use that default behavior
        // otherwise we can try and prefetch it here
        let cache = Turbolinks.controller.cache.get(location);
        if (!cache) {
          cache = Turbolinks.controller.cache.get(`prefetch${location}`);
        }
        if (!cache) {
          const request = new Turbolinks.CachedHttpRequest(null, location, window.location);
          Turbolinks.controller.cache.put(`prefetch${location}`, request);
          return(request.send());
        }
      }
    }
  }
};

document.addEventListener("touchstart", preload);
document.addEventListener("mouseover", preload);

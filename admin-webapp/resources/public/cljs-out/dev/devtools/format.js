// Compiled by ClojureScript 1.10.520 {}
goog.provide('devtools.format');
goog.require('cljs.core');
goog.require('devtools.context');

/**
 * @interface
 */
devtools.format.IDevtoolsFormat = function(){};

devtools.format._header = (function devtools$format$_header(value){
if((((!((value == null)))) && ((!((value.devtools$format$IDevtoolsFormat$_header$arity$1 == null)))))){
return value.devtools$format$IDevtoolsFormat$_header$arity$1(value);
} else {
var x__4433__auto__ = (((value == null))?null:value);
var m__4434__auto__ = (devtools.format._header[goog.typeOf(x__4433__auto__)]);
if((!((m__4434__auto__ == null)))){
return m__4434__auto__.call(null,value);
} else {
var m__4431__auto__ = (devtools.format._header["_"]);
if((!((m__4431__auto__ == null)))){
return m__4431__auto__.call(null,value);
} else {
throw cljs.core.missing_protocol.call(null,"IDevtoolsFormat.-header",value);
}
}
}
});

devtools.format._has_body = (function devtools$format$_has_body(value){
if((((!((value == null)))) && ((!((value.devtools$format$IDevtoolsFormat$_has_body$arity$1 == null)))))){
return value.devtools$format$IDevtoolsFormat$_has_body$arity$1(value);
} else {
var x__4433__auto__ = (((value == null))?null:value);
var m__4434__auto__ = (devtools.format._has_body[goog.typeOf(x__4433__auto__)]);
if((!((m__4434__auto__ == null)))){
return m__4434__auto__.call(null,value);
} else {
var m__4431__auto__ = (devtools.format._has_body["_"]);
if((!((m__4431__auto__ == null)))){
return m__4431__auto__.call(null,value);
} else {
throw cljs.core.missing_protocol.call(null,"IDevtoolsFormat.-has-body",value);
}
}
}
});

devtools.format._body = (function devtools$format$_body(value){
if((((!((value == null)))) && ((!((value.devtools$format$IDevtoolsFormat$_body$arity$1 == null)))))){
return value.devtools$format$IDevtoolsFormat$_body$arity$1(value);
} else {
var x__4433__auto__ = (((value == null))?null:value);
var m__4434__auto__ = (devtools.format._body[goog.typeOf(x__4433__auto__)]);
if((!((m__4434__auto__ == null)))){
return m__4434__auto__.call(null,value);
} else {
var m__4431__auto__ = (devtools.format._body["_"]);
if((!((m__4431__auto__ == null)))){
return m__4431__auto__.call(null,value);
} else {
throw cljs.core.missing_protocol.call(null,"IDevtoolsFormat.-body",value);
}
}
}
});

devtools.format.setup_BANG_ = (function devtools$format$setup_BANG_(){
if(cljs.core.truth_(devtools.format._STAR_setup_done_STAR_)){
return null;
} else {
devtools.format._STAR_setup_done_STAR_ = true;

devtools.format.make_template_fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11260 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11260["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11261 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11261["templating"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11262 = temp__5455__auto____$2;
return (o11262["make_template"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format.make_group_fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11263 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11263["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11264 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11264["templating"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11265 = temp__5455__auto____$2;
return (o11265["make_group"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format.make_reference_fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11266 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11266["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11267 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11267["templating"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11268 = temp__5455__auto____$2;
return (o11268["make_reference"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format.make_surrogate_fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11269 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11269["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11270 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11270["templating"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11271 = temp__5455__auto____$2;
return (o11271["make_surrogate"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format.render_markup_fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11272 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11272["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11273 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11273["templating"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11274 = temp__5455__auto____$2;
return (o11274["render_markup"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format._LT_header_GT__fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11275 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11275["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11276 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11276["markup"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11277 = temp__5455__auto____$2;
return (o11277["_LT_header_GT_"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

devtools.format._LT_standard_body_GT__fn = (function (){var temp__5455__auto__ = (devtools.context.get_root.call(null)["devtools"]);
if(cljs.core.truth_(temp__5455__auto__)){
var o11278 = temp__5455__auto__;
var temp__5455__auto____$1 = (o11278["formatters"]);
if(cljs.core.truth_(temp__5455__auto____$1)){
var o11279 = temp__5455__auto____$1;
var temp__5455__auto____$2 = (o11279["markup"]);
if(cljs.core.truth_(temp__5455__auto____$2)){
var o11280 = temp__5455__auto____$2;
return (o11280["_LT_standard_body_GT_"]);
} else {
return null;
}
} else {
return null;
}
} else {
return null;
}
})();

if(cljs.core.truth_(devtools.format.make_template_fn)){
} else {
throw (new Error("Assert failed: make-template-fn"));
}

if(cljs.core.truth_(devtools.format.make_group_fn)){
} else {
throw (new Error("Assert failed: make-group-fn"));
}

if(cljs.core.truth_(devtools.format.make_reference_fn)){
} else {
throw (new Error("Assert failed: make-reference-fn"));
}

if(cljs.core.truth_(devtools.format.make_surrogate_fn)){
} else {
throw (new Error("Assert failed: make-surrogate-fn"));
}

if(cljs.core.truth_(devtools.format.render_markup_fn)){
} else {
throw (new Error("Assert failed: render-markup-fn"));
}

if(cljs.core.truth_(devtools.format._LT_header_GT__fn)){
} else {
throw (new Error("Assert failed: <header>-fn"));
}

if(cljs.core.truth_(devtools.format._LT_standard_body_GT__fn)){
return null;
} else {
throw (new Error("Assert failed: <standard-body>-fn"));
}
}
});
devtools.format.render_markup = (function devtools$format$render_markup(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11282 = arguments.length;
var i__4731__auto___11283 = (0);
while(true){
if((i__4731__auto___11283 < len__4730__auto___11282)){
args__4736__auto__.push((arguments[i__4731__auto___11283]));

var G__11284 = (i__4731__auto___11283 + (1));
i__4731__auto___11283 = G__11284;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.render_markup.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.render_markup.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.render_markup_fn,args);
});

devtools.format.render_markup.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.render_markup.cljs$lang$applyTo = (function (seq11281){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11281));
});

devtools.format.make_template = (function devtools$format$make_template(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11286 = arguments.length;
var i__4731__auto___11287 = (0);
while(true){
if((i__4731__auto___11287 < len__4730__auto___11286)){
args__4736__auto__.push((arguments[i__4731__auto___11287]));

var G__11288 = (i__4731__auto___11287 + (1));
i__4731__auto___11287 = G__11288;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.make_template.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.make_template.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_template_fn,args);
});

devtools.format.make_template.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.make_template.cljs$lang$applyTo = (function (seq11285){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11285));
});

devtools.format.make_group = (function devtools$format$make_group(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11290 = arguments.length;
var i__4731__auto___11291 = (0);
while(true){
if((i__4731__auto___11291 < len__4730__auto___11290)){
args__4736__auto__.push((arguments[i__4731__auto___11291]));

var G__11292 = (i__4731__auto___11291 + (1));
i__4731__auto___11291 = G__11292;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.make_group.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.make_group.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_group_fn,args);
});

devtools.format.make_group.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.make_group.cljs$lang$applyTo = (function (seq11289){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11289));
});

devtools.format.make_surrogate = (function devtools$format$make_surrogate(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11294 = arguments.length;
var i__4731__auto___11295 = (0);
while(true){
if((i__4731__auto___11295 < len__4730__auto___11294)){
args__4736__auto__.push((arguments[i__4731__auto___11295]));

var G__11296 = (i__4731__auto___11295 + (1));
i__4731__auto___11295 = G__11296;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.make_surrogate.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.make_surrogate.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_surrogate_fn,args);
});

devtools.format.make_surrogate.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.make_surrogate.cljs$lang$applyTo = (function (seq11293){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11293));
});

devtools.format.template = (function devtools$format$template(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11298 = arguments.length;
var i__4731__auto___11299 = (0);
while(true){
if((i__4731__auto___11299 < len__4730__auto___11298)){
args__4736__auto__.push((arguments[i__4731__auto___11299]));

var G__11300 = (i__4731__auto___11299 + (1));
i__4731__auto___11299 = G__11300;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.template.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.template.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_template_fn,args);
});

devtools.format.template.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.template.cljs$lang$applyTo = (function (seq11297){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11297));
});

devtools.format.group = (function devtools$format$group(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11302 = arguments.length;
var i__4731__auto___11303 = (0);
while(true){
if((i__4731__auto___11303 < len__4730__auto___11302)){
args__4736__auto__.push((arguments[i__4731__auto___11303]));

var G__11304 = (i__4731__auto___11303 + (1));
i__4731__auto___11303 = G__11304;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.group.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.group.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_group_fn,args);
});

devtools.format.group.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.group.cljs$lang$applyTo = (function (seq11301){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11301));
});

devtools.format.surrogate = (function devtools$format$surrogate(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11306 = arguments.length;
var i__4731__auto___11307 = (0);
while(true){
if((i__4731__auto___11307 < len__4730__auto___11306)){
args__4736__auto__.push((arguments[i__4731__auto___11307]));

var G__11308 = (i__4731__auto___11307 + (1));
i__4731__auto___11307 = G__11308;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.surrogate.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.surrogate.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_surrogate_fn,args);
});

devtools.format.surrogate.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.surrogate.cljs$lang$applyTo = (function (seq11305){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11305));
});

devtools.format.reference = (function devtools$format$reference(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11316 = arguments.length;
var i__4731__auto___11317 = (0);
while(true){
if((i__4731__auto___11317 < len__4730__auto___11316)){
args__4736__auto__.push((arguments[i__4731__auto___11317]));

var G__11318 = (i__4731__auto___11317 + (1));
i__4731__auto___11317 = G__11318;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((1) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((1)),(0),null)):null);
return devtools.format.reference.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4737__auto__);
});

devtools.format.reference.cljs$core$IFn$_invoke$arity$variadic = (function (object,p__11312){
var vec__11313 = p__11312;
var state_override = cljs.core.nth.call(null,vec__11313,(0),null);
devtools.format.setup_BANG_.call(null);

return cljs.core.apply.call(null,devtools.format.make_reference_fn,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [object,((function (vec__11313,state_override){
return (function (p1__11309_SHARP_){
return cljs.core.merge.call(null,p1__11309_SHARP_,state_override);
});})(vec__11313,state_override))
], null));
});

devtools.format.reference.cljs$lang$maxFixedArity = (1);

/** @this {Function} */
devtools.format.reference.cljs$lang$applyTo = (function (seq11310){
var G__11311 = cljs.core.first.call(null,seq11310);
var seq11310__$1 = cljs.core.next.call(null,seq11310);
var self__4717__auto__ = this;
return self__4717__auto__.cljs$core$IFn$_invoke$arity$variadic(G__11311,seq11310__$1);
});

devtools.format.standard_reference = (function devtools$format$standard_reference(target){
devtools.format.setup_BANG_.call(null);

return devtools.format.make_template_fn.call(null,new cljs.core.Keyword(null,"ol","ol",932524051),new cljs.core.Keyword(null,"standard-ol-style","standard-ol-style",2143825615),devtools.format.make_template_fn.call(null,new cljs.core.Keyword(null,"li","li",723558921),new cljs.core.Keyword(null,"standard-li-style","standard-li-style",413442955),devtools.format.make_reference_fn.call(null,target)));
});
devtools.format.build_header = (function devtools$format$build_header(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11320 = arguments.length;
var i__4731__auto___11321 = (0);
while(true){
if((i__4731__auto___11321 < len__4730__auto___11320)){
args__4736__auto__.push((arguments[i__4731__auto___11321]));

var G__11322 = (i__4731__auto___11321 + (1));
i__4731__auto___11321 = G__11322;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((0) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((0)),(0),null)):null);
return devtools.format.build_header.cljs$core$IFn$_invoke$arity$variadic(argseq__4737__auto__);
});

devtools.format.build_header.cljs$core$IFn$_invoke$arity$variadic = (function (args){
devtools.format.setup_BANG_.call(null);

return devtools.format.render_markup.call(null,cljs.core.apply.call(null,devtools.format._LT_header_GT__fn,args));
});

devtools.format.build_header.cljs$lang$maxFixedArity = (0);

/** @this {Function} */
devtools.format.build_header.cljs$lang$applyTo = (function (seq11319){
var self__4718__auto__ = this;
return self__4718__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq11319));
});

devtools.format.standard_body_template = (function devtools$format$standard_body_template(var_args){
var args__4736__auto__ = [];
var len__4730__auto___11325 = arguments.length;
var i__4731__auto___11326 = (0);
while(true){
if((i__4731__auto___11326 < len__4730__auto___11325)){
args__4736__auto__.push((arguments[i__4731__auto___11326]));

var G__11327 = (i__4731__auto___11326 + (1));
i__4731__auto___11326 = G__11327;
continue;
} else {
}
break;
}

var argseq__4737__auto__ = ((((1) < args__4736__auto__.length))?(new cljs.core.IndexedSeq(args__4736__auto__.slice((1)),(0),null)):null);
return devtools.format.standard_body_template.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4737__auto__);
});

devtools.format.standard_body_template.cljs$core$IFn$_invoke$arity$variadic = (function (lines,rest){
devtools.format.setup_BANG_.call(null);

var args = cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.map.call(null,(function (x){
return new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [x], null);
}),lines)], null),rest);
return devtools.format.render_markup.call(null,cljs.core.apply.call(null,devtools.format._LT_standard_body_GT__fn,args));
});

devtools.format.standard_body_template.cljs$lang$maxFixedArity = (1);

/** @this {Function} */
devtools.format.standard_body_template.cljs$lang$applyTo = (function (seq11323){
var G__11324 = cljs.core.first.call(null,seq11323);
var seq11323__$1 = cljs.core.next.call(null,seq11323);
var self__4717__auto__ = this;
return self__4717__auto__.cljs$core$IFn$_invoke$arity$variadic(G__11324,seq11323__$1);
});


//# sourceMappingURL=format.js.map

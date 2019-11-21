// Compiled by ClojureScript 1.10.520 {}
goog.provide('admin_webapp.core');
goog.require('cljs.core');
goog.require('goog.dom');
goog.require('reagent.core');
cljs.core.println.call(null,"This text is printed from src/admin_webapp/core.cljs. Go ahead and edit it and see reloading in action.");
admin_webapp.core.multiply = (function admin_webapp$core$multiply(a,b){
return (a * b);
});
if((typeof admin_webapp !== 'undefined') && (typeof admin_webapp.core !== 'undefined') && (typeof admin_webapp.core.app_state !== 'undefined')){
} else {
admin_webapp.core.app_state = reagent.core.atom.call(null,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"text","text",-1790561697),"Hello world!"], null));
}
admin_webapp.core.get_app_element = (function admin_webapp$core$get_app_element(){
return goog.dom.getElement("app");
});
admin_webapp.core.hello_world = (function admin_webapp$core$hello_world(){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"div","div",1057191632),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h1","h1",-1896887462),new cljs.core.Keyword(null,"text","text",-1790561697).cljs$core$IFn$_invoke$arity$1(cljs.core.deref.call(null,admin_webapp.core.app_state))], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"h3","h3",2067611163),"Edit this in src/admin_webapp/core.cljs and watch it change!"], null)], null);
});
admin_webapp.core.mount = (function admin_webapp$core$mount(el){
return reagent.core.render_component.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [admin_webapp.core.hello_world], null),el);
});
admin_webapp.core.mount_app_element = (function admin_webapp$core$mount_app_element(){
var temp__5457__auto__ = admin_webapp.core.get_app_element.call(null);
if(cljs.core.truth_(temp__5457__auto__)){
var el = temp__5457__auto__;
return admin_webapp.core.mount.call(null,el);
} else {
return null;
}
});
admin_webapp.core.mount_app_element.call(null);
admin_webapp.core.on_reload = (function admin_webapp$core$on_reload(){
return admin_webapp.core.mount_app_element.call(null);
});

//# sourceMappingURL=core.js.map

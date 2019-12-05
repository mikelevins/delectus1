import { writable } from 'svelte/store';

export const authorization = writable(null);

export function discardAuthorization() {
    authorization.set(null)
}

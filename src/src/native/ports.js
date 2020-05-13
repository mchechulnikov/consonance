const initPorts = app => {
    // local storage
    app.ports.localStorageGetItem.subscribe(key => {
        const value = localStorage.getItem(key);

        if (value)
            app.localStorageGetItemResponse.send([key, value])
    });
    app.ports.localStorageSetItem.subscribe(([key, value]) => {
        localStorage.setItem(key, value);
    });
};

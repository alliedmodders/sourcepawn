public void OnPluginStart() {
    Hell hell;
    hell.DoACrash();
}

methodmap Hell {
    property int DoACrash {
        public get() {
            return 7;
        }
    }
}

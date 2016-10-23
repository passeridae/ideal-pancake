package com.example.andrew.idealpancake;


        import android.content.Context;
        import android.content.Intent;
        import android.net.Uri;
        import android.webkit.JavascriptInterface;
        import android.widget.Toast;
        import android.content.Context;

public class scannerJSI {
    MainActivity mainActivity;

    /** Instantiate the interface and set the context */
    scannerJSI (MainActivity c) {
        mainActivity = c;
    }
    @JavascriptInterface
    public void scanSomething() {
        mainActivity.scanSomething(false);
    }

    @JavascriptInterface
    public void scanText () {
        mainActivity.scanSomething(true);
    }

    @JavascriptInterface
    public void manualISBNSearch(final String isbn) {
        // insert some parsing here
        new Thread ( new Runnable () { public void run () {
       /*     CharSequence text = "Hello toast!";
            int duration = Toast.LENGTH_SHORT;
            Toast toast = Toast.makeText(mainActivity, text, duration);
            toast.show();*/
            mainActivity.lookUp(isbn);
        }}).start();
    }


}

package com.example.andrew.idealpancake;

import android.content.Intent;
import android.net.Uri;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.Window;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import com.andrew.ISBNLookup.ISBNLookup;

public class MainActivity extends AppCompatActivity {
    private WebView webview;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        super.onCreate(savedInstanceState);
        webview = new WebView(this);
        webview.getSettings().setJavaScriptEnabled(true);
        scannerJSI jsi = new scannerJSI(this);
        webview.addJavascriptInterface(jsi, "Android");
        webview.setWebChromeClient(new WebChromeClient());
        setContentView(webview);
        webview.setWebViewClient(new WebViewClient() {
            @Override
            public boolean shouldOverrideUrlLoading(WebView view, String url) {
                if (Uri.parse(url).getHost().equals("192.168.0.12")) {
                    return false;
                } else {
                    Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
                    startActivity(intent);
                    return true;
                }
            }
        });
        webview.loadUrl("http://192.168.0.12:8080/static/add_a_book_m.html");
    }

    public void scanSomething() {
        // I need things done!  Do I have any volunteers?
        Intent intent = new Intent("com.google.zxing.client.android.SCAN");

        // This flag clears the called app from the activity stack, so users arrive in the expected
        // place next time this application is restarted.
        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);

        intent.putExtra("SCAN_MODE", "PRODUCT_MODE");
        startActivityForResult(intent, 0);
    }

    public void onActivityResult(int requestCode, int resultCode, Intent intent) {
        if (requestCode == 0) {
            if (resultCode == RESULT_OK) {
                //  The Intents Fairy has delivered us some data!
                final String contents = intent.getStringExtra("SCAN_RESULT");
                String format = intent.getStringExtra("SCAN_RESULT_FORMAT");
                new Thread ( new Runnable () { public void run () {
                    lookUp(contents);
                }}).start();

                // Handle successful scan
            } else if (resultCode == RESULT_CANCELED) {
                // Handle cancel
            }
        }
    }

    public void lookUp(final String contents) {
        try {
            final String res = ISBNLookup.getBookDetails(contents);
            webview.post(new Runnable() {
                             @Override
                             public void run() {
                                 webview.loadUrl("javascript:returnData('" + res.replace("'", "\\'") + "','" + contents + "')");
                             }
                         });

        } catch (Exception ex) {
            webview.loadUrl("javascript:throwError('"+ex.toString()+"')");
        }
    }
}

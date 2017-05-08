#import "AppDelegate.h"
#import "ViewController.h"
#import <UserNotifications/UserNotifications.h>

// extern void handler(HsStablePtr);
extern void handlerCString(const char * _Nonnull, HsStablePtr);
// extern void handlerCChar(BOOL, HsStablePtr);

@interface AppDelegate ()

@end

HsStablePtr globalHandler = 0;

// HsStablePtr global_willFinishLaunchingWithOptions = 0;
// HsStablePtr global_didFinishLaunchingWithOptions = 0;
// HsStablePtr global_applicationDidBecomeActive = 0;
// HsStablePtr global_applicationDidEnterBackground = 0;
// HsStablePtr global_applicationWillResignActive = 0;
// HsStablePtr global_applicationWillEnterForeground = 0;
// HsStablePtr global_applicationWillTerminate = 0;
// HsStablePtr global_applicationProtectedDataWillBecomeUnavailable = 0;
// HsStablePtr global_applicationProtectedDataDidBecomeUnavailable = 0;
// HsStablePtr global_applicationDidReceiveMemoryWarning = 0;
// HsStablePtr global_applicationSignificantTimeChange = 0;
// BOOL global_requestNotificationAuth = NO;
// unsigned int global_requestedNotificationOptions = 0;
// HsStablePtr global_requestAuthorizationCompletionHandler = 0;
// BOOL global_registerForRemoteNotifications = NO;
HsStablePtr global_didRegisterForRemoteNotificationsWithDeviceTokenCallback = 0;
// HsStablePtr global_didFailToRegisterForRemoteNotifcationsWithError = 0;

@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    // Override point for customization after application launch.
    self.window.rootViewController = [[ViewController alloc] initWithHandler:globalHandler];
    self.window.backgroundColor = [UIColor whiteColor];
    [self.window makeKeyAndVisible];
    UNUserNotificationCenter* center = [UNUserNotificationCenter currentNotificationCenter];
    // if (global_requestNotificationAuth) {
    [center requestAuthorizationWithOptions:(UNAuthorizationOptionAlert + UNAuthorizationOptionSound + UNAuthorizationOptionBadge)
       completionHandler:^(BOOL granted, NSError * _Nullable error) { }
    ];
    // }
    // if (global_registerForRemoteNotifications) {
    [application registerForRemoteNotifications];
    // }
    // handler(global_didFinishLaunchingWithOptions);
    return YES;
}

// - (void)applicationWillResignActive:(UIApplication *)application {
//     // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
//     // Use this method to pause ongoing tasks, disable timers, and invalidate graphics rendering callbacks. Games should use this method to pause the game.
//     handler(global_applicationWillResignActive);
// }

// - (void)applicationDidEnterBackground:(UIApplication *)application {
//     // Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later.
//     // If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
//     handler(global_applicationDidEnterBackground);
// }

// - (BOOL)application:(UIApplication *)application willFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
//   handler(global_willFinishLaunchingWithOptions);
//   return YES;
// }

// - (void)applicationWillEnterForeground:(UIApplication *)application {
//     // Called as part of the transition from the background to the active state; here you can undo many of the changes made on entering the background.
//     handler(global_applicationWillEnterForeground);
// }

// - (void)applicationDidBecomeActive:(UIApplication *)application {
//     // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
//     handler(global_applicationDidBecomeActive);
// }

// - (void)applicationWillTerminate:(UIApplication *)application {
//     // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
//     handler(global_applicationWillTerminate);
// }

- (void)application:(UIApplication *)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData *)deviceToken {
    if ( global_didRegisterForRemoteNotificationsWithDeviceTokenCallback == 0) {
      NSLog(@"didRegister not initialized");
    } else {
      NSString *deviceTokenString = [deviceToken base64EncodedStringWithOptions: 0];
      NSLog(@"%@", deviceTokenString);
      // handlerCString([deviceTokenString UTF8String], global_didRegisterForRemoteNotificationsWithDeviceTokenCallback);
    }
}

// - (void)application:(UIApplication *)application didFailToRegisterForRemoteNotificationsWithError:(NSError *)error {
//     handlerCString([[error  localizedDescription] UTF8String], global_didFailToRegisterForRemoteNotifcationsWithError);
// }

// - (void)applicationProtectedDataWillBecomeUnavailable:(UIApplication *)application {
//     handler(global_applicationProtectedDataWillBecomeUnavailable);
// }

// - (void)applicationProtectedDataDidBecomeAvailable:(UIApplication *)application {
//     handler(global_applicationProtectedDataDidBecomeUnavailable);
// }

// - (void)applicationSignificantTimeChange:(UIApplication *)application {
//     handler(global_applicationSignificantTimeChange);
// }

// - (void)applicationDidReceiveMemoryWarning:(UIApplication *)application {
//     handler(global_applicationDidReceiveMemoryWarning);
// }

@end

void runInWKWebView( HsStablePtr jsaddleHandler,
                     const char * _Nonnull progName,
                     // HsStablePtr hs_willFinishLaunchingWithOptions,
                     // HsStablePtr hs_didFinishLaunchingWithOptions,
                     // HsStablePtr hs_applicationDidBecomeActive,
                     // HsStablePtr hs_applicationDidEnterBackground,
                     // HsStablePtr hs_applicationWillResignActive,
                     // HsStablePtr hs_applicationWillEnterForeground,
                     // HsStablePtr hs_applicationWillTerminate,
                     // HsStablePtr hs_applicationProtectedDataWillBecomeUnavailable,
                     // HsStablePtr hs_applicationProtectedDataDidBecomeUnavailable,
                     // HsStablePtr hs_applicationDidReceiveMemoryWarning,
                     // HsStablePtr hs_applicationSignificantTimeChange,
                     // BOOL hs_requestNotificationAuth,
                     // unsigned int hs_requestedNotificationOptions,
                     // HsStablePtr hs_requestAuthorizationCompletionHandler,
                     // BOOL hs_registerForRemoteNotifications,
                     HsStablePtr hs_didRegisterForRemoteNotificationsWithDeviceTokenCallback) {
                     // HsStablePtr hs_didFailToRegisterForRemoteNotifcationsWithError) {
    @autoreleasepool {
        globalHandler = jsaddleHandler;
        // global_willFinishLaunchingWithOptions = hs_willFinishLaunchingWithOptions;
        // global_didFinishLaunchingWithOptions = hs_didFinishLaunchingWithOptions;
        // global_applicationDidBecomeActive = hs_applicationDidBecomeActive;
        // global_applicationDidEnterBackground = hs_applicationDidEnterBackground;
        // global_applicationWillResignActive = hs_applicationWillResignActive;
        // global_applicationWillEnterForeground = hs_applicationWillEnterForeground;
        // global_applicationWillTerminate = hs_applicationWillTerminate;
        // global_applicationProtectedDataWillBecomeUnavailable = hs_applicationProtectedDataWillBecomeUnavailable;
        // global_applicationProtectedDataDidBecomeUnavailable = hs_applicationProtectedDataDidBecomeUnavailable;
        // global_applicationDidReceiveMemoryWarning = hs_applicationDidReceiveMemoryWarning;
        // global_applicationSignificantTimeChange = hs_applicationSignificantTimeChange;
        // global_requestNotificationAuth = hs_requestNotificationAuth;
        // global_requestedNotificationOptions = hs_requestedNotificationOptions;
        // global_requestAuthorizationCompletionHandler = hs_requestAuthorizationCompletionHandler;
        // global_registerForRemoteNotifications = hs_registerForRemoteNotifications;
        global_didRegisterForRemoteNotificationsWithDeviceTokenCallback = hs_didRegisterForRemoteNotificationsWithDeviceTokenCallback;
        // global_didFailToRegisterForRemoteNotifcationsWithError = hs_didFailToRegisterForRemoteNotifcationsWithError;
        const char * _Nonnull argv [] =  {"", 0};
        UIApplicationMain(0, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}

BOOL openApp(NSURL * url) {
    UIApplication *app = [UIApplication sharedApplication];
    if ([app canOpenURL:url]) {
        [app openURL:url];
        return true;
    }
    return false;
}

// UNAuthorizationOptions authorizationOptionBadge() {
//   return UNAuthorizationOptionBadge;
// }
// 
// UNAuthorizationOptions authorizationOptionSound() {
//   return UNAuthorizationOptionSound;
// }
// 
// UNAuthorizationOptions authorizationOptionAlert() {
//   return UNAuthorizationOptionAlert;
// }
// 
// UNAuthorizationOptions authorizationOptionCarPlay() {
//   return UNAuthorizationOptionCarPlay;
// }

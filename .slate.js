// Configs
S.cfga({
  "defaultToCurrentScreen" : true,
  "windowHintsHeight":"170",
  "windowHintsWidth":"170",
  "secondsBetweenRepeat" : 0.1,
  "checkDefaultsOnLoad" : true,
  "focusCheckWidthMax" : 3000,
  "orderScreensLeftToRight" : true,
  "windowHintsSpread":true,
  "windowHintsShowIcons":true,
  "windowHintsIgnoreHiddenWindows":false,
});

// shortcuts
var hyper = ":cmd;shift;ctrl"
var move = ":cmd;ctrl"

// Monitors
var monTbolt  = "0";
var monLaptop = "1";


S.bnda({
  // Focus Bindings
  ["l"+move] : S.op("focus", { "direction" : "right" }),
  ["h"+move] : S.op("focus", { "direction" : "left" }),
  ["k"+move] : S.op("focus", { "direction" : "up" }),
  ["j"+move] : S.op("focus", { "direction" : "down" }),
//  ["n"+move] : S.op("focus", { "direction" : "behind" }),


  // Window Hints
  "space:ctrl" : S.op("hint",{
    "showNormalHint":"jkl;fdsa"
  }),

  // Switch currently doesn't work well so I'm commenting it out until I fix it.
  "tab:cmd" : S.op("switch"),

  // Relaunch
  ["r"+hyper ]: S.op("relaunch"),

})

;;;; ***********************************************************************
;;;;
;;;; Name:          views.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI viwes of delectus daata
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; macos button types
;;; ---------------------------------------------------------------------
;;; NSButton button types
;;; from enumeration NSButtonType in NSButtonCell

(defparameter +NSButtonTypeMomentaryLight+  0)
(defparameter +NSButtonTypePushOnPushOff+  1)
(defparameter +NSButtonTypeToggle+  2)
(defparameter +NSButtonTypeSwitch+  3)
(defparameter +NSButtonTypeRadio+  4)
(defparameter +NSButtonTypeMomentaryChange+  5)
(defparameter +NSButtonTypeOnOff+  6)
(defparameter +NSButtonTypeMomentaryPushIn+  7)
(defparameter +NSButtonTypeAccelerator+  8)
(defparameter +NSButtonTypeMultiLevelAccelerator+  9)

;;; ---------------------------------------------------------------------
;;; macos button styles
;;; ---------------------------------------------------------------------
;;; NSButton bezel styles
;;; from enumeration NSBezelStyle in NSButtonCell

(defparameter +NSBezelStyleRounded+  1)
(defparameter +NSBezelStyleRegularSquare+  2)
(defparameter +NSBezelStyleDisclosure+  5)
(defparameter +NSBezelStyleShadowlessSquare+  6)
(defparameter +NSBezelStyleCircular+  7)
(defparameter +NSBezelStyleTexturedSquare+  8)
(defparameter +NSBezelStyleHelpButton+  9)
(defparameter +NSBezelStyleSmallSquare+  10)
(defparameter +NSBezelStyleTexturedRounded+  11)
(defparameter +NSBezelStyleRoundRect+  12)
(defparameter +NSBezelStyleRecessed+  13)
(defparameter +NSBezelStyleRoundedDisclosure+  14)
(defparameter +NSBezelStyleInline+  15)

;;; ---------------------------------------------------------------------
;;; macos button images
;;; ---------------------------------------------------------------------
;;; NSImage built-in system image names

(defparameter +NSImageNameActionTemplate+ "NSActionTemplate")
(defparameter +NSImageNameAddTemplate+ "NSAddTemplate")
(defparameter +NSImageNameAdvanced+ "NSAdvanced")
(defparameter +NSImageNameApplicationIcon+ "NSApplicationIcon")
(defparameter +NSImageNameBluetoothTemplate+ "NSBluetoothTemplate")
(defparameter +NSImageNameBonjour+ "NSBonjour")
(defparameter +NSImageNameBookmarksTemplate+ "NSBookmarksTemplate")
(defparameter +NSImageNameCaution+ "NSCaution")
(defparameter +NSImageNameColorPanel+ "NSColorPanel")
(defparameter +NSImageNameColumnViewTemplate+ "NSColumnViewTemplate")
(defparameter +NSImageNameComputer+ "NSComputer")
(defparameter +NSImageNameEnterFullScreenTemplate+ "NSEnterFullScreenTemplate")
(defparameter +NSImageNameEveryone+ "NSEveryone")
(defparameter +NSImageNameExitFullScreenTemplate+ "NSExitFullScreenTemplate")
(defparameter +NSImageNameFlowViewTemplate+ "NSFlowViewTemplate")
(defparameter +NSImageNameFolder+ "NSFolder")
(defparameter +NSImageNameFolderBurnable+ "NSFolderBurnable")
(defparameter +NSImageNameFolderSmart+ "NSFolderSmart")
(defparameter +NSImageNameFollowLinkFreestandingTemplate+ "NSFollowLinkFreestandingTemplate")
(defparameter +NSImageNameFontPanel+ "NSFontPanel")
(defparameter +NSImageNameGoLeftTemplate+ "NSGoLeftTemplate")
(defparameter +NSImageNameGoRightTemplate+ "NSGoRightTemplate")
(defparameter +NSImageNameHomeTemplate+ "NSHomeTemplate")
(defparameter +NSImageNameIChatTheaterTemplate+ "NSIChatTheaterTemplate")
(defparameter +NSImageNameIconViewTemplate+ "NSIconViewTemplate")
(defparameter +NSImageNameInfo+ "NSInfo")
(defparameter +NSImageNameInvalidDataFreestandingTemplate+ "NSInvalidDataFreestandingTemplate")
(defparameter +NSImageNameLeftFacingTriangleTemplate+ "NSLeftFacingTriangleTemplate")
(defparameter +NSImageNameListViewTemplate+ "NSListViewTemplate")
(defparameter +NSImageNameLockLockedTemplate+ "NSLockLockedTemplate")
(defparameter +NSImageNameLockUnlockedTemplate+ "NSLockUnlockedTemplate")
(defparameter +NSImageNameMenuMixedStateTemplate+ "NSMenuMixedStateTemplate")
(defparameter +NSImageNameMenuOnStateTemplate+ "NSMenuOnStateTemplate")
(defparameter +NSImageNameMobileMe+ "NSMobileMe")
(defparameter +NSImageNameMultipleDocuments+ "NSMultipleDocuments")
(defparameter +NSImageNameNetwork+ "NSNetwork")
(defparameter +NSImageNamePathTemplate+ "NSPathTemplate")
(defparameter +NSImageNamePreferencesGeneral+ "NSPreferencesGeneral")
(defparameter +NSImageNameQuickLookTemplate+ "NSQuickLookTemplate")
(defparameter +NSImageNameRefreshFreestandingTemplate+ "NSRefreshFreestandingTemplate")
(defparameter +NSImageNameRefreshTemplate+ "NSRefreshTemplate")
(defparameter +NSImageNameRemoveTemplate+ "NSRemoveTemplate")
(defparameter +NSImageNameRevealFreestandingTemplate+ "NSRevealFreestandingTemplate")
(defparameter +NSImageNameRightFacingTriangleTemplate+ "NSRightFacingTriangleTemplate")
(defparameter +NSImageNameShareTemplate+ "NSShareTemplate")
(defparameter +NSImageNameSlideshowTemplate+ "NSSlideshowTemplate")
(defparameter +NSImageNameSmartBadgeTemplate+ "NSSmartBadgeTemplate")
(defparameter +NSImageNameStatusAvailable+ "NSStatusAvailable")
(defparameter +NSImageNameStatusNone+ "NSStatusNone")
(defparameter +NSImageNameStatusPartiallyAvailable+ "NSStatusPartiallyAvailable")
(defparameter +NSImageNameStatusUnavailable+ "NSStatusUnavailable")
(defparameter +NSImageNameStopProgressFreestandingTemplate+ "NSStopProgressFreestandingTemplate")
(defparameter +NSImageNameStopProgressTemplate+ "NSStopProgressTemplate")
(defparameter +NSImageNameTrashEmpty+ "NSTrashEmpty")
(defparameter +NSImageNameTrashFull+ "NSTrashFull")
(defparameter +NSImageNameUser+ "NSUser")
(defparameter +NSImageNameUserAccounts+ "NSUserAccounts")
(defparameter +NSImageNameUserGroup+ "NSUserGroup")
(defparameter +NSImageNameUserGuest+ "NSUserGuest")

;;; undocumented images
(defparameter +NSAddBookmarkTemplate+ "NSAddBookmarkTemplate")
(defparameter +NSAudioOutputMuteTemplate+ "NSAudioOutputMuteTemplate")
(defparameter +NSAudioOutputVolumeHighTemplate+ "NSAudioOutputVolumeHighTemplate")
(defparameter +NSAudioOutputVolumeLowTemplate+ "NSAudioOutputVolumeLowTemplate")
(defparameter +NSAudioOutputVolumeMedTemplate+ "NSAudioOutputVolumeMedTemplate")
(defparameter +NSAudioOutputVolumeOffTemplate+ "NSAudioOutputVolumeOffTemplate")
(defparameter +NSChildContainerEmptyTemplate+ "NSChildContainerEmptyTemplate")
(defparameter +NSChildContainerTemplate+ "NSChildContainerTemplate")
(defparameter +NSDropDownIndicatorTemplate+ "NSDropDownIndicatorTemplate")
(defparameter +NSGoLeftSmall+ "NSGoLeftSmall")
(defparameter +NSGoRightSmall+ "NSGoRightSmall")
(defparameter +NSMenuMixedStateTemplate+ "NSMenuMixedStateTemplate")
(defparameter +NSMenuOnStateTemplate+ "NSMenuOnStateTemplate")
(defparameter +NSNavEjectButton.normal+ "NSNavEjectButton.normal")
(defparameter +NSNavEjectButton.normalSelected+ "NSNavEjectButton.normalSelected")
(defparameter +NSNavEjectButton.pressed+ "NSNavEjectButton.pressed")
(defparameter +NSNavEjectButton.rollover+ "NSNavEjectButton.rollover")
(defparameter +NSNavEjectButton.small.normal+ "NSNavEjectButton.small.normal")
(defparameter +NSNavEjectButton.small.normalSelected+ "NSNavEjectButton.small.normalSelected")
(defparameter +NSNavEjectButton.small.pressed+ "NSNavEjectButton.small.pressed")
(defparameter +NSNavEjectButton.small.rollover+ "NSNavEjectButton.small.rollover")
(defparameter +NSPathLocationArrow+ "NSPathLocationArrow")
(defparameter +NSPrivateArrowNextTemplate+ "NSPrivateArrowNextTemplate")
(defparameter +NSPrivateArrowPreviousTemplate+ "NSPrivateArrowPreviousTemplate")
(defparameter +NSPrivateChaptersTemplate+ "NSPrivateChaptersTemplate")
(defparameter +NSScriptTemplate+ "NSScriptTemplate")
(defparameter +NSSecurity+ "NSSecurity")
(defparameter +NSStatusAvailableFlat+ "NSStatusAvailableFlat")
(defparameter +NSStatusAway+ "NSStatusAway")
(defparameter +NSStatusIdle+ "NSStatusIdle")
(defparameter +NSStatusNoneFlat+ "NSStatusNoneFlat")
(defparameter +NSStatusOffline+ "NSStatusOffline")
(defparameter +NSStatusPartiallyAvailableFlat+ "NSStatusPartiallyAvailableFlat")
(defparameter +NSStatusUnavailableFlat+ "NSStatusUnavailableFlat")
(defparameter +NSStatusUnknown+ "NSStatusUnknown")
(defparameter +NSSynchronize+ "NSSynchronize")
(defparameter +NSTitlebarEnterFullScreenTemplate+ "NSTitlebarEnterFullScreenTemplate")
(defparameter +NSTitlebarExitFullScreenTemplate+ "NSTitlebarExitFullScreenTemplate")
(defparameter +NSTokenPopDownArrow+ "NSTokenPopDownArrow")
(defparameter +NSFastForwardTemplate+ "NSFastForwardTemplate")
(defparameter +NSPauseTemplate+ "NSPauseTemplate")
(defparameter +NSPlayTemplate+ "NSPlayTemplate")
(defparameter +NSRecordStartTemplate+ "NSRecordStartTemplate")
(defparameter +NSRecordStopTemplate+ "NSRecordStopTemplate")
(defparameter +NSRewindTemplate+ "NSRewindTemplate")
(defparameter +NSSkipAheadTemplate+ "NSSkipAheadTemplate")
(defparameter +NSSkipBackTemplate+ "NSSkipBackTemplate")
(defparameter +NSToolbarBookmarks+ "NSToolbarBookmarks")
(defparameter +NSToolbarClipIndicator+ "NSToolbarClipIndicator")
(defparameter +NSToolbarCustomizeToolbarItemImage+ "NSToolbarCustomizeToolbarItemImage")
(defparameter +NSToolbarFlexibleSpaceItemPaletteRep+ "NSToolbarFlexibleSpaceItemPaletteRep")
(defparameter +NSToolbarMoreTemplate+ "NSToolbarMoreTemplate")
(defparameter +NSToolbarPrintItemImage+ "NSToolbarPrintItemImage")
(defparameter +NSToolbarShowColorsItemImage+ "NSToolbarShowColorsItemImage")
(defparameter +NSToolbarShowFontsItemImage+ "NSToolbarShowFontsItemImage")
(defparameter +NSToolbarSpaceItemPaletteRep+ "NSToolbarSpaceItemPaletteRep")
(defparameter +NSMediaBrowserIcon+ "NSMediaBrowserIcon")
(defparameter +NSMediaBrowserMediaTypeAudio+ "NSMediaBrowserMediaTypeAudio")
(defparameter +NSMediaBrowserMediaTypeAudioTemplate32+ "NSMediaBrowserMediaTypeAudioTemplate32")
(defparameter +NSMediaBrowserMediaTypeMovies+ "NSMediaBrowserMediaTypeMovies")
(defparameter +NSMediaBrowserMediaTypeMoviesTemplate32+ "NSMediaBrowserMediaTypeMoviesTemplate32")
(defparameter +NSMediaBrowserMediaTypePhotos+ "NSMediaBrowserMediaTypePhotos")
(defparameter +NSMediaBrowserMediaTypePhotosTemplate32+ "NSMediaBrowserMediaTypePhotosTemplate32")
(defparameter +NSCMYKButton+ "NSCMYKButton")
(defparameter +NSColorPickerCrayon+ "NSColorPickerCrayon")
(defparameter +NSColorPickerList+ "NSColorPickerList")
(defparameter +NSColorPickerSliders+ "NSColorPickerSliders")
(defparameter +NSColorPickerUser+ "NSColorPickerUser")
(defparameter +NSColorPickerWheel+ "NSColorPickerWheel")
(defparameter +NSColorSwatchResizeDimple+ "NSColorSwatchResizeDimple")
(defparameter +NSGreyButton+ "NSGreyButton")
(defparameter +NSHSBButton+ "NSHSBButton")
(defparameter +NSMagnifyingGlass+ "NSMagnifyingGlass")
(defparameter +NSRGBButton+ "NSRGBButton")
(defparameter +NSSmallMagnifyingGlass+ "NSSmallMagnifyingGlass")
(defparameter +NSFontPanelActionButton+ "NSFontPanelActionButton")
(defparameter +NSFontPanelActionButtonPressed+ "NSFontPanelActionButtonPressed")
(defparameter +NSFontPanelBlurEffect+ "NSFontPanelBlurEffect")
(defparameter +NSFontPanelDropEffect+ "NSFontPanelDropEffect")
(defparameter +NSFontPanelDropEffectPressed+ "NSFontPanelDropEffectPressed")
(defparameter +NSFontPanelEffectsDivider+ "NSFontPanelEffectsDivider")
(defparameter +NSFontPanelMinusIdle+ "NSFontPanelMinusIdle")
(defparameter +NSFontPanelMinusPressed+ "NSFontPanelMinusPressed")
(defparameter +NSFontPanelOpacityEffect+ "NSFontPanelOpacityEffect")
(defparameter +NSFontPanelPaperColour+ "NSFontPanelPaperColour")
(defparameter +NSFontPanelPaperColourPressed+ "NSFontPanelPaperColourPressed")
(defparameter +NSFontPanelPlusIdle+ "NSFontPanelPlusIdle")
(defparameter +NSFontPanelPlusPressed+ "NSFontPanelPlusPressed")
(defparameter +NSFontPanelSliderThumb+ "NSFontPanelSliderThumb")
(defparameter +NSFontPanelSliderThumbPressed+ "NSFontPanelSliderThumbPressed")
(defparameter +NSFontPanelSliderTrack+ "NSFontPanelSliderTrack")
(defparameter +NSFontPanelSplitterKnob+ "NSFontPanelSplitterKnob")
(defparameter +NSFontPanelSpreadEffect+ "NSFontPanelSpreadEffect")
(defparameter +NSFontPanelStrikeEffect+ "NSFontPanelStrikeEffect")
(defparameter +NSFontPanelStrikeEffectPressed+ "NSFontPanelStrikeEffectPressed")
(defparameter +NSFontPanelTextColour+ "NSFontPanelTextColour")
(defparameter +NSFontPanelTextColourPressed+ "NSFontPanelTextColourPressed")
(defparameter +NSFontPanelUnderlineEffect+ "NSFontPanelUnderlineEffect")
(defparameter +NSFontPanelUnderlineEffectPressed+ "NSFontPanelUnderlineEffectPressed")
(defparameter +NSDatePickerCalendarArrowLeft+ "NSDatePickerCalendarArrowLeft")
(defparameter +NSDatePickerCalendarArrowRight+ "NSDatePickerCalendarArrowRight")
(defparameter +NSDatePickerCalendarHome+ "NSDatePickerCalendarHome")
(defparameter +NSDatePickerClockCenter+ "NSDatePickerClockCenter")
(defparameter +NSDatePickerClockFace+ "NSDatePickerClockFace")
(defparameter +NSTextRulerCenterTab+ "NSTextRulerCenterTab")
(defparameter +NSTextRulerDecimalTab+ "NSTextRulerDecimalTab")
(defparameter +NSTextRulerFirstLineIndent+ "NSTextRulerFirstLineIndent")
(defparameter +NSTextRulerIndent+ "NSTextRulerIndent")
(defparameter +NSTextRulerLeftTab+ "NSTextRulerLeftTab")
(defparameter +NSTextRulerRightTab+ "NSTextRulerRightTab")
(defparameter +NSTouchBarAddDetailTemplate+ "NSTouchBarAddDetailTemplate")
(defparameter +NSTouchBarAddTemplate+ "NSTouchBarAddTemplate")
(defparameter +NSTouchBarAlarmTemplate+ "NSTouchBarAlarmTemplate")
(defparameter +NSTouchBarAudioInputMuteTemplate+ "NSTouchBarAudioInputMuteTemplate")
(defparameter +NSTouchBarAudioInputTemplate+ "NSTouchBarAudioInputTemplate")
(defparameter +NSTouchBarAudioOutputMuteTemplate+ "NSTouchBarAudioOutputMuteTemplate")
(defparameter +NSTouchBarAudioOutputVolumeHighTemplate+ "NSTouchBarAudioOutputVolumeHighTemplate")
(defparameter +NSTouchBarAudioOutputVolumeLowTemplate+ "NSTouchBarAudioOutputVolumeLowTemplate")
(defparameter +NSTouchBarAudioOutputVolumeMediumTemplate+ "NSTouchBarAudioOutputVolumeMediumTemplate")
(defparameter +NSTouchBarAudioOutputVolumeOffTemplate+ "NSTouchBarAudioOutputVolumeOffTemplate")
(defparameter +NSTouchBarBookmarksTemplate+ "NSTouchBarBookmarksTemplate")
(defparameter +NSTouchBarColorPickerFill+ "NSTouchBarColorPickerFill")
(defparameter +NSTouchBarColorPickerFont+ "NSTouchBarColorPickerFont")
(defparameter +NSTouchBarColorPickerStroke+ "NSTouchBarColorPickerStroke")
(defparameter +NSTouchBarCommunicationAudioTemplate+ "NSTouchBarCommunicationAudioTemplate")
(defparameter +NSTouchBarCommunicationVideoTemplate+ "NSTouchBarCommunicationVideoTemplate")
(defparameter +NSTouchBarComposeTemplate+ "NSTouchBarComposeTemplate")
(defparameter +NSTouchBarDeleteTemplate+ "NSTouchBarDeleteTemplate")
(defparameter +NSTouchBarDownloadTemplate+ "NSTouchBarDownloadTemplate")
(defparameter +NSTouchBarEnterFullScreenTemplate+ "NSTouchBarEnterFullScreenTemplate")
(defparameter +NSTouchBarExitFullScreenTemplate+ "NSTouchBarExitFullScreenTemplate")
(defparameter +NSTouchBarFastForwardTemplate+ "NSTouchBarFastForwardTemplate")
(defparameter +NSTouchBarFolderCopyToTemplate+ "NSTouchBarFolderCopyToTemplate")
(defparameter +NSTouchBarFolderMoveToTemplate+ "NSTouchBarFolderMoveToTemplate")
(defparameter +NSTouchBarFolderTemplate+ "NSTouchBarFolderTemplate")
(defparameter +NSTouchBarGetInfoTemplate+ "NSTouchBarGetInfoTemplate")
(defparameter +NSTouchBarGoBackTemplate+ "NSTouchBarGoBackTemplate")
(defparameter +NSTouchBarGoDownTemplate+ "NSTouchBarGoDownTemplate")
(defparameter +NSTouchBarGoForwardTemplate+ "NSTouchBarGoForwardTemplate")
(defparameter +NSTouchBarGoUpTemplate+ "NSTouchBarGoUpTemplate")
(defparameter +NSTouchBarHistoryTemplate+ "NSTouchBarHistoryTemplate")
(defparameter +NSTouchBarIconViewTemplate+ "NSTouchBarIconViewTemplate")
(defparameter +NSTouchBarListViewTemplate+ "NSTouchBarListViewTemplate")
(defparameter +NSTouchBarMailTemplate+ "NSTouchBarMailTemplate")
(defparameter +NSTouchBarNewFolderTemplate+ "NSTouchBarNewFolderTemplate")
(defparameter +NSTouchBarNewMessageTemplate+ "NSTouchBarNewMessageTemplate")
(defparameter +NSTouchBarOpenInBrowserTemplate+ "NSTouchBarOpenInBrowserTemplate")
(defparameter +NSTouchBarPauseTemplate+ "NSTouchBarPauseTemplate")
(defparameter +NSTouchBarPlayPauseTemplate+ "NSTouchBarPlayPauseTemplate")
(defparameter +NSTouchBarPlayTemplate+ "NSTouchBarPlayTemplate")
(defparameter +NSTouchBarQuickLookTemplate+ "NSTouchBarQuickLookTemplate")
(defparameter +NSTouchBarRecordStartTemplate+ "NSTouchBarRecordStartTemplate")
(defparameter +NSTouchBarRecordStopTemplate+ "NSTouchBarRecordStopTemplate")
(defparameter +NSTouchBarRefreshTemplate+ "NSTouchBarRefreshTemplate")
(defparameter +NSTouchBarRewindTemplate+ "NSTouchBarRewindTemplate")
(defparameter +NSTouchBarRotateLeftTemplate+ "NSTouchBarRotateLeftTemplate")
(defparameter +NSTouchBarRotateRightTemplate+ "NSTouchBarRotateRightTemplate")
(defparameter +NSTouchBarSearchTemplate+ "NSTouchBarSearchTemplate")
(defparameter +NSTouchBarShareTemplate+ "NSTouchBarShareTemplate")
(defparameter +NSTouchBarSidebarTemplate+ "NSTouchBarSidebarTemplate")
(defparameter +NSTouchBarSkipAhead15SecondsTemplate+ "NSTouchBarSkipAhead15SecondsTemplate")
(defparameter +NSTouchBarSkipAhead30SecondsTemplate+ "NSTouchBarSkipAhead30SecondsTemplate")
(defparameter +NSTouchBarSkipAheadTemplate+ "NSTouchBarSkipAheadTemplate")
(defparameter +NSTouchBarSkipBack15SecondsTemplate+ "NSTouchBarSkipBack15SecondsTemplate")
(defparameter +NSTouchBarSkipBack30SecondsTemplate+ "NSTouchBarSkipBack30SecondsTemplate")
(defparameter +NSTouchBarSkipBackTemplate+ "NSTouchBarSkipBackTemplate")
(defparameter +NSTouchBarSkipToEndTemplate+ "NSTouchBarSkipToEndTemplate")
(defparameter +NSTouchBarSkipToStartTemplate+ "NSTouchBarSkipToStartTemplate")
(defparameter +NSTouchBarSlideshowTemplate+ "NSTouchBarSlideshowTemplate")
(defparameter +NSTouchBarTagIconTemplate+ "NSTouchBarTagIconTemplate")
(defparameter +NSTouchBarTextBoldTemplate+ "NSTouchBarTextBoldTemplate")
(defparameter +NSTouchBarTextBoxTemplate+ "NSTouchBarTextBoxTemplate")
(defparameter +NSTouchBarTextCenterAlignTemplate+ "NSTouchBarTextCenterAlignTemplate")
(defparameter +NSTouchBarTextItalicTemplate+ "NSTouchBarTextItalicTemplate")
(defparameter +NSTouchBarTextJustifiedAlignTemplate+ "NSTouchBarTextJustifiedAlignTemplate")
(defparameter +NSTouchBarTextLeftAlignTemplate+ "NSTouchBarTextLeftAlignTemplate")
(defparameter +NSTouchBarTextListTemplate+ "NSTouchBarTextListTemplate")
(defparameter +NSTouchBarTextRightAlignTemplate+ "NSTouchBarTextRightAlignTemplate")
(defparameter +NSTouchBarTextStrikethroughTemplate+ "NSTouchBarTextStrikethroughTemplate")
(defparameter +NSTouchBarTextUnderlineTemplate+ "NSTouchBarTextUnderlineTemplate")
(defparameter +NSTouchBarUserAddTemplate+ "NSTouchBarUserAddTemplate")
(defparameter +NSTouchBarUserGroupTemplate+ "NSTouchBarUserGroupTemplate")
(defparameter +NSTouchBarUserTemplate+ "NSTouchBarUserTemplate")
(defparameter +NSTouchBarVolumeDownTemplate+ "NSTouchBarVolumeDownTemplate")
(defparameter +NSTouchBarVolumeUpTemplate+ "NSTouchBarVolumeUpTemplate")
(defparameter +NSArrowCursor+ "NSArrowCursor")
(defparameter +NSClosedHandCursor+ "NSClosedHandCursor")
(defparameter +NSCopyDragCursor+ "NSCopyDragCursor")
(defparameter +NSCrosshairCursor+ "NSCrosshairCursor")
(defparameter +NSGenericDragCursor+ "NSGenericDragCursor")
(defparameter +NSHandCursor+ "NSHandCursor")
(defparameter +NSIBeamCursor+ "NSIBeamCursor")
(defparameter +NSLinkDragCursor+ "NSLinkDragCursor")
(defparameter +NSMoveCursor+ "NSMoveCursor")
(defparameter +NSResizeLeftCursor+ "NSResizeLeftCursor")
(defparameter +NSResizeLeftRightCursor+ "NSResizeLeftRightCursor")
(defparameter +NSResizeRightCursor+ "NSResizeRightCursor")
(defparameter +NSTruthBottomLeftResizeCursor+ "NSTruthBottomLeftResizeCursor")
(defparameter +NSTruthBottomRightResizeCursor+ "NSTruthBottomRightResizeCursor")
(defparameter +NSTruthHResizeCursor+ "NSTruthHResizeCursor")
(defparameter +NSTruthHorizontalResizeCursor+ "NSTruthHorizontalResizeCursor")
(defparameter +NSTruthTopLeftResizeCursor+ "NSTruthTopLeftResizeCursor")
(defparameter +NSTruthTopRightResizeCursor+ "NSTruthTopRightResizeCursor")
(defparameter +NSTruthVResizeCursor+ "NSTruthVResizeCursor")
(defparameter +NSTruthVerticalResizeCursor+ "NSTruthVerticalResizeCursor")
(defparameter +NSWaitCursor+ "NSWaitCursor")

;;; ---------------------------------------------------------------------
;;; macos utils
;;; ---------------------------------------------------------------------

(defmethod set-mac-button-style ((button push-button)(style integer))
  (let* ((rep (capi-cocoa-library::representation-main-view (capi::find-representation-for-pane button))))
    (objc:invoke rep "setBezelStyle:" style)))

(defmethod set-mac-button-image ((button push-button)(image-name string))
  (let* ((rep (capi-cocoa-library::representation-main-view (capi::find-representation-for-pane button))))
    (objc:invoke rep "setImage:" (objc:invoke "NSImage" "imageNamed:" image-name))))

;;; ---------------------------------------------------------------------
;;; Delectus style
;;; ---------------------------------------------------------------------

(defparameter *delectus-application-button-style* +NSBezelStyleTexturedSquare+)

;;; ---------------------------------------------------------------------
;;; list-items-pane
;;; ---------------------------------------------------------------------

(define-interface list-items-pane ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (total-items :accessor total-items :initform 0 :initarg :total-items)
   (items-per-page :accessor items-per-page :initform 50 :initarg :items-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (items-pane multi-column-list-panel :reader items-pane
               :alternating-background t
               :item-print-function (lambda (it)
                                      (if (null it) "" it))
               :items nil
               :columns '((:title "Item"))
               :callback-type :item-interface
               :selection-callback 'handle-item-selection)
   (filter-pane text-input-pane :search-field "Filter")
   (previous-button push-button :reader previous-button :text ""
                    :external-min-width 28 :external-max-width 28
                    :external-min-height 32 :external-max-height 32
                    :callback #'handle-previous-button-click)
   (next-button push-button :reader next-button :text ""
                :external-min-width 28 :external-max-width 28
                :external-min-height 32 :external-max-height 32
                :callback #'handle-next-button-click)
   (item-count-pane title-pane :reader item-count-pane)
   (item-range-pane title-pane :reader item-range-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (pager-layout row-layout '(previous-button item-range-pane next-button) :adjust :center)
   (controls-layout row-layout '(item-count-pane nil filter-pane nil pager-layout)
                    :ratios '(3 3 18 3 6)
                    :adjust :center)
   (main-layout column-layout '(items-pane controls-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 600 :height 400
   :title "Delectus"))

(defmethod initialize-instance :after ((pane list-items-pane) &rest initargs 
                                       &key (show-metadata nil) &allow-other-keys)
  (setf (total-items pane)
        (delectus::count-latest-items (dbpath pane)))
  (update-list-display pane :show-metadata show-metadata))

;;; fix up the widget styles
(defmethod capi:interface-display :before ((pane list-items-pane))
  (set-mac-button-style (previous-button pane)
                        *delectus-application-button-style*)
  (set-mac-button-image (previous-button pane)
                        +NSImageNameGoLeftTemplate+)
  (set-mac-button-style (next-button pane)
                        *delectus-application-button-style*)
  (set-mac-button-image (next-button pane)
                        +NSImageNameGoRightTemplate+))

(defmethod total-pages ((pane list-items-pane))
  (ceiling (total-items pane)
           (items-per-page pane)))

(defmethod update-list-display ((pane list-items-pane) &rest initargs 
                                &key (show-metadata nil) &allow-other-keys)
  (let* ((metadata-column-count (length delectus::+metadata-column-labels+))
         (column-info (delectus::get-column-info (dbpath pane)))
         (userdata-column-info (subseq column-info metadata-column-count))
         (latest-column-data (delectus::get-latest-columns (dbpath pane)))
         (latest-userdata (mapcar #'delectus::from-json (delectus::op-userdata latest-column-data)))
         (list-name-op (delectus::get-latest-listname (dbpath pane)))
         (listname (or (delectus::op-name list-name-op) "Untitled list"))
         (latest-items (delectus::get-latest-items (dbpath pane) 
                                                   :offset (* (items-per-page pane)
                                                              (current-page pane))
                                                   :limit (items-per-page pane)))
         (itemdata (mapcar #'delectus::op-userdata latest-items))
         (column-names (mapcar (lambda (ud)(fset:@ ud :|name|)) latest-userdata))
         ;; allow plenty of room for strings in columns
         (column-widths (mapcar #'(lambda (x)(+ 4 x))
                                (delectus::get-userdata-column-widths (dbpath pane))))
         (column-specs (mapcar (lambda (name width) `(:title ,name :default-width (:character ,width)))
                               column-names column-widths)))
    (setf (interface-title pane) listname)
    (modify-multi-column-list-panel-columns (items-pane pane) :columns column-specs)
    (setf (title-pane-text (item-count-pane pane)) 
          (format nil " ~:D pages (~:D items)"
                  (total-pages pane) (total-items pane)))
    (setf (title-pane-text (item-range-pane pane)) 
          (format nil "Page ~D"
                  (1+ (current-page pane))))
    (setf (collection-items (items-pane pane))
          itemdata)))

(defun dec-list-page (list-items-pane)
  (let ((next-page (1- (current-page list-items-pane))))
    (when (>= next-page 0)
      (decf (current-page list-items-pane))))
  (update-list-display list-items-pane :show-metadata nil))

(defun inc-list-page (list-items-pane)
  (let* ((itemcount (total-items list-items-pane))
         (next-start-index (* (items-per-page list-items-pane)
                              (1+ (current-page list-items-pane)))))
    (when (< next-start-index itemcount)
      (incf (current-page list-items-pane))))
  (update-list-display list-items-pane :show-metadata nil))

(defun handle-previous-button-click (data interface)
  (dec-list-page interface))

(defun handle-next-button-click (data interface)
  (inc-list-page interface))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $zippath))))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $moviespath))))

;;; (setf $screen (convert-to-screen))
;;; (describe $screen)

;;; opening test data
;;; (defparameter $words1k-path "/Users/mikel/Desktop/wordtest1k.delectus2")
;;; ~0.07sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words1k-path))))

;;; (defparameter $words10k-path "/Users/mikel/Desktop/wordtest10k.delectus2")
;;; ~0.14sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words10k-path))))

;;; (defparameter $words100k-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; ~0.8sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words100k-path))))
;;; ~0.4 sec to page
;;; (time (inc-list-page $win))


;;; ---------------------------------------------------------------------
;;; list-item-card
;;; ---------------------------------------------------------------------

(define-interface list-item-card ()
  ;; -- slots ---------------------------------------------
  ((columns-data :accessor columns-data :initform nil :initarg :columns-data)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '()
                :reader main-layout
                :border 8
                :gap 8))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 400 :height 300))

(defmethod initialize-instance :after ((card list-item-card) &rest initargs 
                                       &key &allow-other-keys)
  (let* ((column-value-pairs (mapcar #'cons
                                     (columns-data card)
                                     (item-data card)))
         (sorted-pairs (sort column-value-pairs
                             (lambda (left right)
                               (< (getf (car left) :|order|)
                                  (getf (car right) :|order|)))))
         (entry-views (mapcar (lambda (it)
                                (let* ((coldata (car it))
                                       (colname (getf coldata :|name|))
                                       (colvalue (cdr it))
                                       (entrypane (make-instance 'title-pane
                                                                 :title-position :left
                                                                 :title (format nil "~A" colname)
                                                                 :title-font (gp:make-font-description 
                                                                              :size 12
                                                                              :slant :italic)
                                                                 :text (format nil "~A" colvalue)
                                                                 :font (gp:make-font-description 
                                                                        :size 16
                                                                        :slant :roman))))
                                  entrypane))
                              sorted-pairs)))
    (setf (layout-description (main-layout card))
          entry-views)))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (progn (setf $columns (delectus::get-latest-userdata-columns-data $zippath)) 'done))
;;; (time (progn (setf $items (mapcar #'delectus::op-userdata (delectus::get-latest-items $zippath))) 'done))
;;; (length $items)
;;; (setf $win (contain (make-instance 'list-item-card :columns-data $columns :item-data (elt $items 0))))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (progn (setf $columns (delectus::get-latest-userdata-columns-data $moviespath)) 'done))
;;; (time (progn (setf $items (delectus::get-latest-items-userdata $moviespath)) 'done))
;;; (length $items)
;;; (setf $win (contain (make-instance 'list-item-card :columns-data $columns :item-data (elt $items 0))))


;;; ---------------------------------------------------------------------
;;; card-list
;;; ---------------------------------------------------------------------


;;;; ***********************************************************************
;;;;
;;;; Name:          macos-constants.lisp
;;;; Project:       delectus 2
;;;; Purpose:       macos styles, icons, and so forth
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)
(in-readtable :standard)

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
;;; Delectus button style
;;; ---------------------------------------------------------------------

(defparameter *delectus-application-button-style* +NSBezelStyleTexturedSquare+)

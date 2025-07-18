use core::fmt;
use std::mem::transmute;

// ASCII character
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(u8)]
pub enum AChar {
    #[default]
    Null = 0,
    StartOfHeading = 1,
    StartOfText = 2,
    EndOfText = 3,
    EndOfTransmission = 4,
    Enquiry = 5,
    Acknowledge = 6,
    Bell = 7,
    Backspace = 8,
    CharacterTabulation = 9,
    LineFeed = 10,
    LineTabulation = 11,
    FormFeed = 12,
    CarriageReturn = 13,
    ShiftOut = 14,
    ShiftIn = 15,
    DataLinkEscape = 16,
    DeviceControlOne = 17,
    DeviceControlTwo = 18,
    DeviceControlThree = 19,
    DeviceControlFour = 20,
    NegativeAcknowledge = 21,
    SynchronousIdle = 22,
    EndOfTransmissionBlock = 23,
    Cancel = 24,
    EndOfMedium = 25,
    Substitute = 26,
    Escape = 27,
    InformationSeparatorFour = 28,
    InformationSeparatorThree = 29,
    InformationSeparatorTwo = 30,
    InformationSeparatorOne = 31,
    Space = 32,
    ExclamationMark = 33,
    QuotationMark = 34,
    NumberSign = 35,
    DollarSign = 36,
    PercentSign = 37,
    Ampersand = 38,
    Apostrophe = 39,
    LeftParenthesis = 40,
    RightParenthesis = 41,
    Asterisk = 42,
    PlusSign = 43,
    Comma = 44,
    HyphenMinus = 45,
    FullStop = 46,
    Solidus = 47,
    Digit0 = 48,
    Digit1 = 49,
    Digit2 = 50,
    Digit3 = 51,
    Digit4 = 52,
    Digit5 = 53,
    Digit6 = 54,
    Digit7 = 55,
    Digit8 = 56,
    Digit9 = 57,
    Colon = 58,
    Semicolon = 59,
    LessThanSign = 60,
    EqualsSign = 61,
    GreaterThanSign = 62,
    QuestionMark = 63,
    CommercialAt = 64,
    CapitalA = 65,
    CapitalB = 66,
    CapitalC = 67,
    CapitalD = 68,
    CapitalE = 69,
    CapitalF = 70,
    CapitalG = 71,
    CapitalH = 72,
    CapitalI = 73,
    CapitalJ = 74,
    CapitalK = 75,
    CapitalL = 76,
    CapitalM = 77,
    CapitalN = 78,
    CapitalO = 79,
    CapitalP = 80,
    CapitalQ = 81,
    CapitalR = 82,
    CapitalS = 83,
    CapitalT = 84,
    CapitalU = 85,
    CapitalV = 86,
    CapitalW = 87,
    CapitalX = 88,
    CapitalY = 89,
    CapitalZ = 90,
    LeftSquareBracket = 91,
    ReverseSolidus = 92,
    RightSquareBracket = 93,
    CircumflexAccent = 94,
    LowLine = 95,
    GraveAccent = 96,
    SmallA = 97,
    SmallB = 98,
    SmallC = 99,
    SmallD = 100,
    SmallE = 101,
    SmallF = 102,
    SmallG = 103,
    SmallH = 104,
    SmallI = 105,
    SmallJ = 106,
    SmallK = 107,
    SmallL = 108,
    SmallM = 109,
    SmallN = 110,
    SmallO = 111,
    SmallP = 112,
    SmallQ = 113,
    SmallR = 114,
    SmallS = 115,
    SmallT = 116,
    SmallU = 117,
    SmallV = 118,
    SmallW = 119,
    SmallX = 120,
    SmallY = 121,
    SmallZ = 122,
    LeftCurlyBracket = 123,
    VerticalLine = 124,
    RightCurlyBracket = 125,
    Tilde = 126,
    Delete = 127,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct AStr([AChar]);

impl AChar {
    #[inline]
    pub const fn from_u8(b: u8) -> Option<Self> {
        if b <= 127 {
            // SAFETY: Just checked that `b` is in-range
            Some(unsafe { Self::from_u8_unchecked(b) })
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// `b` must be in `0..=127`, or else this is UB.
    #[inline]
    pub const unsafe fn from_u8_unchecked(b: u8) -> Self {
        // SAFETY: Our safety precondition is that `b` is in-range.
        unsafe { transmute(b) }
    }

    /// Gets this ASCII character as a byte.
    #[inline]
    pub const fn to_u8(self) -> u8 {
        self as u8
    }

    /// Gets this ASCII character as a `char` Unicode Scalar Value.
    #[inline]
    pub const fn to_char(self) -> char {
        self as u8 as char
    }

    /// Views this ASCII character as a one-code-unit UTF-8 `str`.
    #[inline]
    pub const fn as_str(&self) -> &str {
        AStr::from_ref(self).as_str()
    }
}

impl<'a> From<&'a [AChar]> for &'a AStr {
    fn from(value: &'a [AChar]) -> Self {
        AStr::new(value)
    }
}

impl AStr {
    #[inline]
    pub const fn from_ascii(bytes: &[u8]) -> Option<&Self> {
        if bytes.is_ascii() {
            // SAFETY: Checked on previous line
            Some(unsafe { Self::from_ascii_unchecked(bytes) })
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// All bytes in `bytes` must be in `0..=127`, or else this is UB.
    #[inline]
    pub const unsafe fn from_ascii_unchecked(bytes: &[u8]) -> &Self {
        // SAFETY: Our safety precondition is that `bytes` are in-range.
        unsafe { transmute(bytes) }
    }

    #[inline]
    pub const fn new(slice: &[AChar]) -> &Self {
        // SAFETY: AStr has the same layout as slice of `AChar`s
        unsafe { transmute(slice) }
    }

    #[inline]
    pub const fn as_slice(&self) -> &[AChar] {
        // SAFETY: AStr has the same layout as slice of `AChar`s
        unsafe { transmute(self) }
    }

    #[inline]
    pub const fn from_ref(c: &AChar) -> &Self {
        Self::new(core::slice::from_ref(c))
    }

    /// Views this slice of ASCII characters as a UTF-8 `str`.
    #[inline]
    pub const fn as_str(&self) -> &str {
        let ascii_ptr: *const Self = self;

        let str_ptr = ascii_ptr as *const str;

        // SAFETY: Each ASCII codepoint in UTF-8 is encoded as one single-byte
        // code unit having the same value as the ASCII byte.
        unsafe { &*str_ptr }
    }

    /// Views this slice of ASCII characters as a slice of `u8` bytes.
    #[inline]
    pub const fn as_bytes(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

#[inline]
pub const fn array_as_ascii<const N: usize>(arr: [u8; N]) -> Option<[AChar; N]> {
    if arr.is_ascii() {
        Some(unsafe { array_as_ascii_unchecked(arr) })
    } else {
        None
    }
}

/// # Safety
///
/// All bytes in `arr` must be in `0..=127`, or else this is UB.
#[inline]
pub const unsafe fn array_as_ascii_unchecked<const N: usize>(arr: [u8; N]) -> [AChar; N] {
    let byte_ptr: *const [u8; N] = &arr;

    let ascii_ptr = byte_ptr as *const [AChar; N];

    // SAFETY: The caller promised all the bytes are ASCII
    unsafe { *ascii_ptr }
}

impl fmt::Display for AChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <str as fmt::Display>::fmt(self.as_str(), f)
    }
}

impl fmt::Debug for AChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use AChar::{Apostrophe, Null, ReverseSolidus as Backslash};

        fn backslash(a: AChar) -> ([AChar; 6], usize) {
            ([Apostrophe, Backslash, a, Apostrophe, Null, Null], 4)
        }

        let (buf, len) = match self {
            AChar::Null => backslash(AChar::Digit0),

            AChar::CharacterTabulation => backslash(AChar::SmallT),

            AChar::CarriageReturn => backslash(AChar::SmallR),

            AChar::LineFeed => backslash(AChar::SmallN),

            AChar::ReverseSolidus => backslash(AChar::ReverseSolidus),

            AChar::Apostrophe => backslash(AChar::Apostrophe),

            _ if self.to_u8().is_ascii_control() => {
                const HEX_DIGITS: [AChar; 16] = array_as_ascii(*b"0123456789abcdef").unwrap();

                let byte = self.to_u8();

                let hi = HEX_DIGITS[usize::from(byte >> 4)];

                let lo = HEX_DIGITS[usize::from(byte & 0xf)];

                (
                    [Apostrophe, Backslash, AChar::SmallX, hi, lo, Apostrophe],
                    6,
                )
            }

            _ => ([Apostrophe, *self, Apostrophe, Null, Null, Null], 3),
        };

        f.write_str(AStr::new(&buf[..len]).as_str())
    }
}

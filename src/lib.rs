use std::fmt::Display;

use bitflags::bitflags;
use itertools::Itertools;
use num_rational::Rational32;
use thiserror::Error;

trait StringExt {
    fn trim_surrounding(&self, c: char) -> &Self;
}

impl StringExt for str {
    fn trim_surrounding(&self, c: char) -> &str {
        self.strip_prefix(c)
            .and_then(|s| s.strip_suffix(c))
            .unwrap_or(self)
    }
}

/// Error type for CUE parsing
#[derive(Error, Debug)]
pub enum CUEParseError {
    /// The timestamp is invalid
    #[error("invalid timestamp: {0}")]
    InvalidTimeStamp(String),
    /// The required entries (title and performer) are not present in the CUE
    /// track
    #[error("missing {0} entry in cue track: {1}")]
    MissingEntry(&'static str, String),
    /// Invalid tag (first word) occurred in line
    #[error("unsupported tag in line: {0}")]
    InvalidTag(String),
    /// No value (word after tag) are present in line
    #[error("missing value in line: {0}")]
    MissingValue(String),
    /// The line cannot be parsed
    #[error("invalid line: {0}")]
    Invalid(String),
    /// Some file lines are not associated with track definitions
    #[error("not all files are used by tracks")]
    FilesNotUsed,
    /// The catalog value is not a 13-digit UPC/EAN code
    #[error("the catalog code should be a 13-digit UPC/EAN code")]
    InvalidCatalog(String),
}

pub type FileID = usize;

/// Main struct for representing a CUE file
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CUEFile {
    /// The file names referenced by the CUE file
    pub files:      Vec<String>,
    /// The title of the album
    pub title:      String,
    /// The performer of the album
    pub performer:  String,
    /// The catalog ID of the album
    pub catalog:    Option<String>,
    /// The name of the CD-TEXT file
    pub text_file:  Option<String>,
    /// The songwriter of the album
    pub songwriter: Option<String>,
    /// Represents which file this track resides in, it is guaranteed that
    /// [`FileID`]s for all [`CUETrack`]s uses up 0..`CUEFile.files.len()`
    pub tracks:     Vec<(FileID, CUETrack)>,
    /// REM comment lines
    pub comments:   Vec<String>,
}

impl TryFrom<&str> for CUEFile {
    type Error = CUEParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let value = value.strip_prefix('\u{feff}').unwrap_or(value);

        let mut files = vec![];
        let mut cur_file_id = None;
        let mut title = None;
        let mut performer = None;
        let mut catalog = None;
        let mut text_file = None;
        let mut songwriter = None;
        let mut comments = vec![];
        let mut tracks = vec![];

        let mut lines = value.lines().peekable();
        loop {
            let Some(line) = lines.next() else {
                break;
            };

            if !line.starts_with("  ") {
                let mut line_split = line.splitn(2, ' ');
                let tag = line_split.next().unwrap();

                match tag {
                    "FILE" => {
                        let mut split = line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .rsplitn(2, ' ');
                        let file_name = split.nth(1).unwrap().trim_surrounding('"');
                        files.push(file_name.to_owned());
                        cur_file_id = Some(files.len() - 1);
                    }
                    "TITLE" => {
                        title = Some(
                            line_split
                                .next()
                                .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                                .trim_surrounding('"'),
                        )
                    }
                    "PERFORMER" => {
                        performer = Some(
                            line_split
                                .next()
                                .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                                .trim_surrounding('"'),
                        )
                    }
                    "CATALOG" => {
                        let catalog_val = line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?;
                        if catalog_val.len() != 13
                            || catalog_val.chars().any(|c| !c.is_ascii_digit())
                        {
                            Err(CUEParseError::InvalidCatalog(catalog_val.to_owned()))?
                        }

                        catalog = Some(catalog_val.to_owned())
                    }
                    "CDTEXTFILE" => {
                        text_file = Some(
                            line_split
                                .next()
                                .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                                .trim_surrounding('"')
                                .to_owned(),
                        )
                    }
                    "SONGWRITER" => {
                        songwriter = Some(
                            line_split
                                .next()
                                .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                                .trim_surrounding('"')
                                .to_owned(),
                        )
                    }
                    "REM" => comments.push(
                        line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .to_owned(),
                    ),
                    _ => Err(CUEParseError::InvalidTag(line.to_owned()))?,
                }
            } else {
                if cur_file_id.is_none() {
                    Err(CUEParseError::Invalid(line.to_owned()))?
                }

                let line = line.trim();
                if line.starts_with("TRACK") {
                    let mut track_str = vec![];
                    while let Some(line) = lines.peek() {
                        if !line.starts_with("  ") || line.trim().starts_with("TRACK") {
                            break;
                        } else {
                            track_str.push(lines.next().unwrap().trim());
                        }
                    }
                    if !track_str.is_empty() {
                        let track_str = track_str.join("\n");
                        let track = CUETrack::try_from(track_str.as_str())?;
                        tracks.push((cur_file_id.unwrap(), track));
                    }
                } else {
                    Err(CUEParseError::InvalidTag(line.to_owned()))?
                }
            }
        }

        if files.is_empty() {
            Err(CUEParseError::MissingEntry("file", value.to_owned()))?
        }
        let title = title
            .map(|s| s.to_owned())
            .ok_or_else(|| CUEParseError::MissingEntry("title", value.to_owned()))?;
        let performer = performer
            .map(|s| s.to_owned())
            .ok_or_else(|| CUEParseError::MissingEntry("performer", value.to_owned()))?;

        let mut file_ids = tracks
            .iter()
            .map(|(track_file_id, _)| *track_file_id)
            .collect::<Vec<_>>();
        file_ids.dedup();

        if !file_ids
            .into_iter()
            .zip(0..files.len())
            .all(|(track_file_id, file_id)| track_file_id == file_id)
        {
            Err(CUEParseError::FilesNotUsed)?
        }

        Ok(Self {
            files,
            title,
            performer,
            catalog,
            text_file,
            songwriter,
            tracks,
            comments,
        })
    }
}

impl Display for CUEFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let comments_str = self
            .comments
            .iter()
            .map(|comment| format!("REM {comment}"))
            .join("\n");
        let comments_str = if comments_str.is_empty() {
            None
        } else {
            Some(comments_str)
        };

        let title_str = format!(r#"TITLE "{}""#, self.title);
        let performer_str = format!(r#"PERFORMER "{}""#, self.performer);
        let catalog_str = self
            .catalog
            .as_ref()
            .map(|catalog| format!("CATALOG {catalog}"));
        let text_file_str = self
            .text_file
            .as_ref()
            .map(|text_file| format!(r#"CDTEXTFILE "{text_file}""#));
        let songwriter_str = self
            .songwriter
            .as_ref()
            .map(|songwriter| format!(r#"SONGWRITER "{songwriter}""#));

        let file_strs = self
            .files
            .iter()
            .map(|file| format!(r#"FILE "{}" WAVE"#, file))
            .collect::<Vec<_>>();
        let track_strs = self
            .tracks
            .iter()
            .enumerate()
            .map(|(i, (file_id, t))| {
                let track_header = format!("  TRACK {:02} AUDIO", i + 1);
                let track_content = t.to_string();

                (file_id, format!("{track_header}\n{track_content}"))
            })
            .collect::<Vec<_>>();

        let mut file_track_strs = vec![];
        for (file_id, file_str) in file_strs.into_iter().enumerate() {
            let track_part = track_strs
                .iter()
                .filter(|(id, _)| **id == file_id)
                .map(|(_, s)| s)
                .join("\n");

            file_track_strs.push(format!("{file_str}\n{track_part}"));
        }
        let file_track_strs = file_track_strs.into_iter().join("\n");

        let str = [
            comments_str,
            Some(title_str),
            Some(performer_str),
            catalog_str,
            text_file_str,
            songwriter_str,
            Some(file_track_strs),
        ]
        .into_iter()
        .flatten()
        .join("\n");
        write!(f, "{}", str)
    }
}

bitflags! {
    /// Sub code flags for tracks used in CUE
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TrackFlags: u8 {
        const DCP = 0x0;
        const CH4 = 0x1;
        const PRE = 0x2;
        const SCMS = 0x4;
    }
}

impl From<&str> for TrackFlags {
    /// Non-known flags are ignored
    fn from(s: &str) -> Self {
        let mut flags = TrackFlags::empty();
        for s in s.split(' ') {
            flags |= match s {
                "DCH" => TrackFlags::DCP,
                "4CH" => TrackFlags::CH4,
                "PRE" => TrackFlags::PRE,
                "SCMS" => TrackFlags::SCMS,
                _ => TrackFlags::empty(),
            }
        }
        flags
    }
}

impl Display for TrackFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dcp_str = if self.contains(TrackFlags::DCP) {
            Some("DCP")
        } else {
            None
        };
        let ch4_str = if self.contains(TrackFlags::CH4) {
            Some("4CH")
        } else {
            None
        };
        let pre_str = if self.contains(TrackFlags::PRE) {
            Some("PRE")
        } else {
            None
        };
        let scms_str = if self.contains(TrackFlags::SCMS) {
            Some("SCMS")
        } else {
            None
        };
        let str = [dcp_str, ch4_str, pre_str, scms_str]
            .into_iter()
            .flatten()
            .join(" ");

        write!(f, "{str}")
    }
}

/// struct describing a CUE track
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CUETrack {
    /// The title of the track
    pub title:      String,
    /// The performer of the track
    pub performer:  Option<String>,
    /// The sub code flags of the track
    pub flags:      Option<TrackFlags>,
    /// The ISRC of the track
    pub isrc:       Option<String>,
    /// The post gap of the track
    pub post_gap:   Option<CUETimeStamp>,
    /// The pre gap of the track
    pub pre_gap:    Option<CUETimeStamp>,
    /// The songwriter of the track
    pub songwriter: Option<String>,
    /// The index values of the track
    pub indices:    Vec<(u8, CUETimeStamp)>,
    /// REM comments of the track
    pub comments:   Vec<String>,
}

impl TryFrom<&str> for CUETrack {
    type Error = CUEParseError;

    /// Try from lines describing CUE track **without** the "TRACK XX AUDIO"
    /// line
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut title = None;
        let mut performer = None;
        let mut flags = None;
        let mut isrc = None;
        let mut post_gap = None;
        let mut pre_gap = None;
        let mut songwriter = None;
        let mut indices = vec![];
        let mut comments = vec![];

        for line in value.lines() {
            let mut line_split = line.splitn(2, ' ');

            let tag = line_split.next().unwrap();
            match tag {
                "TITLE" => {
                    title = Some(
                        line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .trim_surrounding('"'),
                    )
                }
                "PERFORMER" => {
                    performer = Some(
                        line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .trim_surrounding('"')
                            .to_owned(),
                    )
                }
                "FLAGS" => {
                    let flags_val = line_split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?;
                    flags = Some(TrackFlags::from(flags_val));
                }
                "ISRC" => {
                    isrc = Some(
                        line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .trim_surrounding('"')
                            .to_owned(),
                    )
                }
                "POSTGAP" => {
                    let post_gap_str = line_split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?;
                    post_gap = Some(CUETimeStamp::try_from(post_gap_str)?)
                }
                "PREGAP" => {
                    let pre_gap_str = line_split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?;
                    pre_gap = Some(CUETimeStamp::try_from(pre_gap_str)?)
                }
                "SONGWRITER" => {
                    songwriter = Some(
                        line_split
                            .next()
                            .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                            .trim_surrounding('"')
                            .to_owned(),
                    )
                }
                "INDEX" => {
                    let mut split = line_split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                        .split(' ');
                    let index_i = split
                        .next()
                        .unwrap()
                        .parse::<u8>()
                        .map_err(|_| CUEParseError::Invalid(line.to_owned()))?;
                    let index_ts = split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                        .try_into()?;
                    indices.push((index_i, index_ts))
                }
                "REM" => comments.push(
                    line_split
                        .next()
                        .ok_or_else(|| CUEParseError::MissingValue(line.to_owned()))?
                        .to_owned(),
                ),
                _ => Err(CUEParseError::InvalidTag(line.to_owned()))?,
            }
        }

        let title = title
            .map(|s| s.to_owned())
            .ok_or_else(|| CUEParseError::MissingEntry("title", value.to_owned()))?;

        Ok(Self {
            title,
            performer,
            flags,
            isrc,
            post_gap,
            pre_gap,
            songwriter,
            indices,
            comments,
        })
    }
}

impl Display for CUETrack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let flags_str = self
            .flags
            .as_ref()
            .map(|flags| format!("    FLAGS {}", flags));
        let title_str = format!(r#"    TITLE "{}""#, self.title);
        let performer_str = self
            .performer
            .as_ref()
            .map(|performer| format!(r#"    PERFORMER "{}""#, performer));
        let isrc_str = self.isrc.as_ref().map(|isrc| format!("    ISRC {}", isrc));
        let pregap_str = self
            .pre_gap
            .as_ref()
            .map(|pregap| format!("    PREGAP {}", pregap));
        let indices_str = self
            .indices
            .iter()
            .map(|(i, ts)| format!("    INDEX {:02} {}", i, ts))
            .join("\n");
        let comments_str = self
            .comments
            .iter()
            .map(|comment| format!("    REM {}", comment))
            .join("\n");
        let comments_str = if comments_str.is_empty() {
            None
        } else {
            Some(comments_str)
        };

        let str = [
            flags_str,
            Some(title_str),
            performer_str,
            pregap_str,
            isrc_str,
            Some(indices_str),
            comments_str,
        ]
        .into_iter()
        .flatten()
        .join("\n");
        write!(f, "{}", str)
    }
}

/// Represents a CUE timestamp, the value in seconds can be obtained by
/// converting it into [`Rational32`]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct CUETimeStamp {
    minutes:   u8,
    seconds:   u8,
    fractions: u8,
}

impl TryFrom<&str> for CUETimeStamp {
    type Error = CUEParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let err = || CUEParseError::InvalidTimeStamp(value.to_owned());

        let split = value.split(':').collect::<Vec<_>>();
        if split.len() != 3 {
            Err(err())?
        }

        let numbers = split
            .into_iter()
            .map(|s| s.parse::<u8>().map_err(|_| err()))
            .collect::<Result<Vec<_>, _>>()?;
        if numbers.iter().any(|&n| n >= 100) {
            Err(err())?
        }

        Ok(Self {
            minutes:   numbers[0],
            seconds:   numbers[1],
            fractions: numbers[2],
        })
    }
}

impl Display for CUETimeStamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:02}:{:02}:{:02}",
            self.minutes, self.seconds, self.fractions
        )
    }
}

/// Converts a [`CUETimeStamp`] into a [`Rational32`], in seconds.
impl From<CUETimeStamp> for Rational32 {
    fn from(ts: CUETimeStamp) -> Self {
        Rational32::from_integer(ts.minutes as i32 * 60)
            + Rational32::from_integer(ts.seconds as _)
            + Rational32::new(ts.fractions as i32, 75)
    }
}

-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types
import Data.List (partition)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec = partition isSelected
  where
    isSelected t =
      let
        -- A test is selected if it matches the include criteria (or if there are no include criteria)
        -- and does not match any exclude criteria.
        isIncluded = null (fsIncludes spec) || matchesAny (fsIncludes spec) t
        isNotExcluded = not (matchesAny (fsExcludes spec) t)
      in
        isIncluded && isNotExcluded

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny criteria test =
  any (matchesCriterion False test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion useRegex test criterion = do
  not useRegex  -- Regex matching not implemented.
  &&
    case criterion of
      -- @ByAny@ matches if the criterion matches the test name, category, or tag.
      ByAny crit -> tcdName test == crit || tcdCategory test == crit || crit `elem` tcdTags test
      -- @ByCategory@ matches if the criterion matches the test category.
      ByCategory cat -> tcdCategory test == cat
      -- @ByTag@ matches if the criterion tag is in the test's tags.
      ByTag tag -> tag `elem` tcdTags test

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace

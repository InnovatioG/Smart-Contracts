

# Redeemers

## Campaign

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    | PolicyRedeemerMintCampaignToken PolicyRedeemerMintCampaignTokenType
    | PolicyRedeemerBurnCampaignToken PolicyRedeemerBurnCampaignTokenType


data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerFundsAdd ValidatorRedeemerFundsAddType
    | ValidatorRedeemerFundsMerge ValidatorRedeemerFundsMergeType
    | ValidatorRedeemerFundsDelete ValidatorRedeemerFundsDeleteType
    | ValidatorRedeemerFundsCollect ValidatorRedeemerFundsCollectType
    | ValidatorRedeemerInitializeCampaign ValidatorRedeemerInitializeCampaignType
    | ValidatorRedeemerReachedCampaign ValidatorRedeemerReachedCampaignType
    | ValidatorRedeemerNotReachedCampaign ValidatorRedeemerNotReachedCampaignType
    | ValidatorRedeemerMilestoneAprobe ValidatorRedeemerMilestoneAprobeType
    | ValidatorRedeemerMilestoneReprobe ValidatorRedeemerMilestoneReprobeType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType

## CampaignFunds

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType


data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerSell ValidatorRedeemerSellType
    | ValidatorRedeemerGetBack ValidatorRedeemerGetBackType
    | ValidatorRedeemerCollect ValidatorRedeemerCollectType
    | ValidatorRedeemerMerge ValidatorRedeemerMergeType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

## Protocol

data ValidatorRedeemer
    = ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

## Script

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

type ValidatorRedeemer = ValidatorRedeemerDelete




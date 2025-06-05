# INNOVATIO – ON-CHAIN TECHNICAL SPECIFICATION

## Introduction and General Overview

The Innovatio protocol implements a decentralized crowdfunding platform based on the Cardano blockchain, combining the security and transparency of smart contracts with the usability of a modern application. Projects raise funds securely and transparently, protecting investors’ interests via a verifiable milestones system.

---

## Key On-Chain Concepts

### Protocol

Refers to all Cardano smart contracts deployed on-chain, which manage the core rules and permissions for campaigns and funds.

### Campaign

A crowdfunding project exists on-chain as a set of UTXOs controlled by smart contracts, with fundraising goals, current state, milestone configuration, transaction history, and authorized management wallets stored in datums.

### Blockchain / On-Chain

The environment where smart contracts reside and operate, datums are stored, and all transactions are performed immutably and verifiably. All “on-chain” operations are direct interactions with the Cardano blockchain.

### Smart Contract (Script)

A Cardano smart contract (script) is a program deployed on-chain that defines specific rules for digital asset management. Innovatio uses three primary contracts:

* Protocol contract (central admin/governance)
* Campaign contract (per project, state/milestones/fundraising)
* Funds contract (manages assets, token distribution, milestone fund release)

Each has its own validation logic and permissions.

### Datum

On-chain state object. Each contract type (Protocol, Campaign, Funds) maintains its own datum, encoding its current state and permissions.

### UTXO (Unspent Transaction Output)

Core ledger model in Cardano. Each UTXO represents:

* Digital assets (ADA, tokens)
* A datum (arbitrary data structure)
* A script address (which controls spending)

Every on-chain operation consumes and creates UTXOs. Campaigns and their funds are represented as specific UTXOs, each with their datum and assets.

### Transactions

Atomic on-chain operations which:

* Consume existing UTXOs
* Create new UTXOs
* Mint or burn tokens
* Update datums
* Require smart contract validation
* Are immutable once confirmed
* Incur ADA fees

#### Example: Buying tokens

* Consumes the campaign’s funds UTXO
* Creates a new UTXO with updated datum
* Transfers ADA from campaign funds
* Delivers tokens to the buyer

---

## System Architecture: On-Chain Layer

### Blockchain (On-Chain)

Where all smart contracts are deployed, datums and assets are stored, and all protocol state transitions are enforced.

#### On-Chain Smart Contract Components

* **Protocol Contract**: Stores the list of admin wallets and controls campaign creation and validation. Centralized rule enforcement.
* **Campaign Contract**: Each project has a campaign contract storing essential parameters, milestone structure, and operational logic (goals, state, authorized wallets).
* **Funds Contract**: Handles deposits, withdrawals, and progressive release of funds to project managers upon milestone achievement.

---

## Protocol: On-Chain Representation

### Protocol Datum Structure

* `pdProtocolVersion` (Integer): Protocol datum version
* `pdAdmins` (\[String]): List of payment public key hashes (Core Team)
* `pdTokenAdminPolicy_CS` (String): Policy hash for protocol admin tokens
* `pdMinADA` (Integer): Minimum ADA required in the protocol UTXO

#### Main On-Chain Responsibilities

* Governance: admin wallet management, admin token control, rule enforcement
* Configuration: protocol version, min ADA requirements, token/validator policies
* Validation: operation verification, access control via tokens, immutable state registry

#### On-Chain Protocol Interactions

* **Initialize Protocol Contracts (TX):** Create initial protocol datum on-chain with default parameters
* **Publish Protocol Contracts (TX):** Deploy contracts by creating reference UTXOs
* **Remove Protocol Contracts (TX):** Remove contract UTXOs, recovering reserved ADA (only if no campaigns exist)

---

## Campaigns: On-Chain Representation

### Campaign Datum Structure

* `cdCampaignVersion` (Integer): Datum version
* `cdCampaignPolicy_CS` (String): Campaign token policy hash
* `cdCampaignFundsPolicyID_CS` (String): Funds token policy hash
* `cdAdmins` (\[String]): Payment public key hashes of administrators
* `cdTokenAdminPolicy_CS` (String): Admin token policy hash
* `cdMint_CampaignToken` (Boolean): Whether the campaign mints its own tokens
* `cdCampaignToken_CS` (String): Campaign token policy hash
* `cdCampaignToken_TN` (String): Campaign token name
* `cdCampaignToken_PriceADA` (Integer): Price per campaign token (in ADA)
* `cdRequestedMaxADA` (Integer): Maximum ADA to raise
* `cdRequestedMinADA` (Integer): Minimum ADA to raise
* `cdFundedADA` (Integer): Total ADA raised so far
* `cdCollectedADA` (Integer): ADA withdrawn by admins
* `cdBeginAt` (Date): Campaign start date
* `cdDeadline` (Date): Campaign end date
* `cdStatus` (CampaignStatus): Current status (see below)
* `cdMilestones` (\[CampaignMilestones]): List of milestone objects
* `cdFundsCount` (Integer): Total funds UTXOs
* `cdFundsIndex` (Integer): Funds index for new UTXO IDs
* `cdMinADA` (Integer): Minimum ADA in UTXO

#### Milestone Structure (`CampaignMilestones`)

* `cmEstimatedDeliveryDate` (Date): (Informational only)
* `cmPercentage` (Integer): Percentage of total funds for milestone
* `cmStatus` (MilestoneStatus): Created, Success, Failed

---

## On-Chain Campaign States

**Status field in datums is used by contracts to control allowed actions (token sale, ADA refund, milestone withdrawals, etc.)**

### CampaignStatus in Datum

* `CsCreated`: just created
* `CsInitialized`: tokens available for sale
* `CsReached`: minimum funds reached
* `CsNotReached`: minimum not reached
* `CsFailedMilestone`: failed milestone

### MilestoneStatus in Datum

* `MsCreated`: just created
* `MsSuccess`: approved
* `MsFailed`: failed

---

## Campaign Lifecycle: On-Chain Process

### Main Process

1. **Creation:** Manager wallet creates campaign, sets up admins, goals, milestones. Campaign datum is initialized.
2. **Activation:** Campaign contract is published and initialized on-chain.
3. **Fundraising:** Users buy tokens, contributing ADA; campaign’s funds UTXOs are updated with each contribution.
4. **Result Determination:** At end date, admins/protocol mark campaign as `CsReached` (if min goal met) or `CsNotReached`.
5. **Milestone Execution:** If successful, managers may access funds for milestone 1. Progress is validated via milestone datums.
6. **Release/Refund:** On success, funds are released milestone by milestone; on failure, users may return tokens to reclaim ADA.

#### Security Mechanisms

* If minimum goal is not reached or milestones fail, investors can recover funds.
* All state and transactions are fully on-chain and auditable.

---

## On-Chain Campaign Interactions

### Contract Deployment

* **Create Campaign Contracts (DB)**
* **Initialize Campaign Contracts (TX)**
* **Publish Campaign Contracts (TX)**
* **Remove Campaign Contracts (TX)**

### Campaign Management (TX)

* **Launch Campaign (TX):** Activates fundraising (sets status to `CsInitialized`)
* **Invest (TX):** User buys tokens (updates funds UTXO)
* **Campaign Reached (TX):** Marks `CsReached` if min met
* **Campaign Not Reached (TX):** Marks `CsNotReached` if min not met
* **Update Campaign Min ADA (TX):** Adjusts min ADA required in campaign UTXO
* **Delete Campaign (TX):** Removes campaign if no ADA/tokens present

### Failed Campaign Management (TX)

* **Return Tokens (TX):** Investors can return tokens to reclaim ADA in case of `CsNotReached` or `CsFailedMilestone`

### Funds Management (TX)

* **Add Campaign Funds UTxO (TX):** Adds a new funds UTXO for concurrency
* **Deposit/Withdraw Campaign Tokens (TX):** Mint/burn tokens for sale
* **Merge/Balance Fund UTxOs (TX):** Consolidate or redistribute tokens
* **Update Funds Min ADA (TX):** Adjust ADA required in fund UTXOs
* **Delete Fund UTxOs (TX):** Remove empty funds UTxOs

### Milestone Management (TX)

* **Approve Milestone (TX):** Protocol admin marks a milestone as passed (`MsSuccess`)
* **Fail Milestone (TX):** Protocol marks milestone and campaign as failed (`MsFailed`, `CsFailedMilestone`)
* **Collect Funds (TX):** Managers collect funds for achieved milestone

---

## Role-Based On-Chain Interactions

* **Protocol Team**: Can deploy, initialize, and remove contracts; approve/reject/validate campaigns; mark milestones.
* **Campaign Managers**: Can launch campaigns, manage funds, present milestones.
* **Investors**: Can invest (buy tokens), return tokens for ADA (if campaign fails).

---

## On-Chain State Machine

(For each state, only the smart contract controls transitions via on-chain transactions. No off-chain override is possible.)

* Created → (Initialized by launch) → CsInitialized (Countdown/Fundraising)
* CsInitialized → (Goal reached) → CsReached
* CsInitialized → (Goal not reached) → CsNotReached
* CsReached → (Milestone Success/Fail) → MsSuccess/MsFailed
* CsFailedMilestone → Refund available
* CsNotReached → Refund available

---

## Appendix: Example Data Structures (Types)

```haskell
-- Protocol Datum
data ProtocolDatum = ProtocolDatum {
  pdProtocolVersion        :: Integer,
  pdAdmins                 :: [PubKeyHash],
  pdTokenAdminPolicy_CS    :: CurrencySymbol,
  pdMinADA                 :: Integer
}

-- Campaign Datum
data CampaignDatum = CampaignDatum {
  cdCampaignVersion        :: Integer,
  cdCampaignPolicy_CS      :: CurrencySymbol,
  cdCampaignFundsPolicyID_CS :: CurrencySymbol,
  cdAdmins                 :: [PubKeyHash],
  cdTokenAdminPolicy_CS    :: CurrencySymbol,
  cdMint_CampaignToken     :: Bool,
  cdCampaignToken_CS       :: CurrencySymbol,
  cdCampaignToken_TN       :: TokenName,
  cdCampaignToken_PriceADA :: Integer,
  cdRequestedMaxADA        :: Integer,
  cdRequestedMinADA        :: Integer,
  cdFundedADA              :: Integer,
  cdCollectedADA           :: Integer,
  cdBeginAt                :: POSIXTime,
  cdDeadline               :: POSIXTime,
  cdStatus                 :: CampaignStatus,
  cdMilestones             :: [CampaignMilestone],
  cdFundsCount             :: Integer,
  cdFundsIndex             :: Integer,
  cdMinADA                 :: Integer
}

-- Milestone Status
data MilestoneStatus = MsCreated | MsSuccess | MsFailed
```

---

## Notes

* All stateful operations and business logic for campaigns, milestones, fundraising, refunds, and roles are fully enforced on-chain.
* All transitions and validation are dictated by the contract logic, not off-chain components.
* Off-chain code (not included here) can only interact via submitting transactions as allowed by the contracts.


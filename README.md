# Birthsafe Smart Contract

A Stacks blockchain smart contract that incentivizes institutional delivery through conditional token rewards. The Safe Birth Token (SBT) encourages expectant mothers to deliver in verified healthcare facilities.

## Contract Overview

Birthsafe creates a token-based incentive system where:
- Verified hospitals can register births
- Mothers receive SBT tokens for institutional deliveries
- Rewards are time-bound and must be claimed within a specific period
- Hospital performance and maternal health records are tracked

## Features

### Core Functionality
- **Hospital Registration**: Only contract owner can register and verify hospitals
- **Birth Registration**: Verified hospitals register institutional births
- **Token Rewards**: Mothers earn SBT tokens for verified institutional births
- **Time-Bound Claims**: Rewards expire after a set number of blocks
- **Record Tracking**: Maintains maternal and hospital delivery statistics

### Token Details
- **Name**: Safe Birth Token
- **Symbol**: SBT
- **Decimals**: 6
- **Default Reward**: 1,000,000 tokens per birth

## Usage Instructions

### For Contract Owner

#### Register a Hospital
```clarity
(contract-call? .Birthsafe register-hospital "City General Hospital" "123 Main St, City")
```

#### Verify/Unverify Hospital
```clarity
(contract-call? .Birthsafe verify-hospital 'SP1234...HOSPITAL true)
```

#### Adjust Reward Amount
```clarity
(contract-call? .Birthsafe set-reward-amount u2000000)
```

#### Set Reward Expiry Period
```clarity
(contract-call? .Birthsafe set-reward-expiry u288000)
```

### For Hospitals

#### Register a Birth
```clarity
(contract-call? .Birthsafe register-birth "BIRTH001" 'SP1234...MOTHER u3200 "2024-01-15")
```

### For Mothers

#### Claim Birth Reward
```clarity
(contract-call? .Birthsafe claim-birth-reward "BIRTH001")
```

#### Transfer Tokens
```clarity
(contract-call? .Birthsafe transfer u500000 tx-sender 'SP1234...RECIPIENT none)
```

### Read-Only Functions

#### Check Token Balance
```clarity
(contract-call? .Birthsafe get-balance 'SP1234...ADDRESS)
```

#### Get Birth Information
```clarity
(contract-call? .Birthsafe get-birth "BIRTH001")
```

#### Check Reward Eligibility
```clarity
(contract-call? .Birthsafe can-claim-reward "BIRTH001")
```

#### Get Hospital Details
```clarity
(contract-call? .Birthsafe get-hospital 'SP1234...HOSPITAL)
```

#### Get Maternal Records
```clarity
(contract-call? .Birthsafe get-maternal-record 'SP1234...MOTHER)
```

#### Get Contract Statistics
```clarity
(contract-call? .Birthsafe get-birth-statistics)
```

## Data Structures

### Hospital Record
- `name`: Hospital name (max 64 characters)
- `location`: Hospital address (max 128 characters)
- `verified`: Verification status
- `registration-block`: Block when registered

### Birth Record
- `mother`: Mother's principal address
- `hospital`: Hospital's principal address
- `birth-block`: Block when birth was registered
- `verified`: Birth verification status
- `reward-claimed`: Whether reward has been claimed
- `birth-weight`: Baby's birth weight in grams
- `birth-date`: Birth date string

### Maternal Record
- `total-births`: Total number of births
- `institutional-births`: Number of institutional births
- `total-rewards`: Total SBT tokens earned
- `last-birth-block`: Block of most recent birth

### Hospital Statistics
- `total-deliveries`: Total births registered
- `successful-deliveries`: Births with claimed rewards
- `total-rewards-distributed`: Total SBT tokens distributed

## Error Codes

- `u100`: Owner-only function
- `u101`: Record not found
- `u102`: Record already exists
- `u103`: Invalid hospital
- `u104`: Invalid birth data
- `u105`: Reward already claimed
- `u106`: Insufficient funds
- `u107`: Reward claim period expired

## Development

### Testing
Run tests using Clarinet:
```bash
clarinet test
```

### Deployment
Deploy to testnet:
```bash
clarinet deployments apply -p testnet
```

## Security Considerations

- Only verified hospitals can register births
- Mothers must claim their own rewards
- Time-bound reward claims prevent indefinite liability
- Contract owner has administrative privileges
- Emergency withdrawal function for contract maintenance

## Impact

Birthsafe incentivizes:
- Increased institutional delivery rates
- Better maternal health outcomes
- Healthcare facility accountability
- Data-driven policy decisions
- Community health awareness

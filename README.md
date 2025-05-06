# FitNest Workout Incentives

FitNest is a blockchain-powered fitness incentive system that motivates users to maintain consistent home workout routines through token rewards, milestones, and community challenges.

## Overview

FitNest creates a gamified fitness experience by:
- Rewarding users with tokens for completing verified workouts
- Increasing rewards based on workout streaks and duration
- Offering milestone achievements with bonus rewards
- Enabling community challenges with stake-based competitions
- Managing a token economy for fitness incentives

## Architecture

The system is built around a core smart contract that handles:

```mermaid
graph TD
    A[User Registration] --> B[Workout Recording]
    B --> C[Workout Verification]
    C --> D[Token Rewards]
    D --> E[Milestone Tracking]
    D --> F[Streak Calculation]
    G[Community Challenges] --> H[Challenge Participation]
    H --> I[Challenge Rewards]
    J[Token Economy] --> K[Token Transfers]
    J --> L[Challenge Staking]
```

Core components:
- User profiles with workout history and statistics
- Workout verification system with authorized verifiers
- Token reward calculation based on streaks and duration
- Milestone achievement system
- Community challenges with staking mechanism

## Contract Documentation

### FitNest Core Contract

The main contract (`fitnest-core`) manages all core functionality:

#### Key Features
- User registration and profile management
- Workout recording and verification
- Token minting and distribution
- Streak tracking and bonus calculations
- Milestone achievement system
- Community challenge management

#### Access Control
- Contract owner: Can manage verifiers and end challenges
- Authorized verifiers: Can verify workouts and create challenges
- Users: Can record workouts, join challenges, and transfer tokens

## Getting Started

### Prerequisites
- Clarinet
- Stacks wallet for deployment

### Basic Usage

1. Register as a user:
```clarity
(contract-call? .fitnest-core register-user)
```

2. Record a workout:
```clarity
(contract-call? .fitnest-core record-workout u30 "cardio")
```

3. Get user profile:
```clarity
(contract-call? .fitnest-core get-user-profile tx-sender)
```

## Function Reference

### User Management
- `register-user()`: Register a new user
- `get-user-profile(principal)`: Get user's fitness profile

### Workout Management
- `record-workout(uint, string-ascii)`: Record a new workout
- `verify-workout(principal, uint)`: Verify a completed workout
- `get-workout(principal, uint)`: Get workout details

### Token Operations
- `get-token-balance(principal)`: Check token balance
- `transfer-tokens(principal, uint)`: Transfer tokens to another user

### Challenges
- `create-challenge(string-ascii, string-utf8, uint, uint, uint)`: Create new challenge
- `join-challenge(uint)`: Join an existing challenge
- `record-challenge-workout(uint, uint)`: Record workout for challenge

## Development

### Testing
1. Clone the repository
2. Run Clarinet console:
```bash
clarinet console
```
3. Execute test commands:
```clarity
(contract-call? .fitnest-core register-user)
```

### Local Development
1. Install Clarinet
2. Initialize project:
```bash
clarinet new fitnest-project
```
3. Copy contract code to `contracts/fitnest-core.clar`

## Security Considerations

### Limitations
- Workout verification relies on authorized verifiers
- Challenge rewards are distributed after challenge completion
- Token transfers are irreversible

### Best Practices
- Only use authorized verifiers for workout validation
- Verify challenge parameters before joining
- Maintain sufficient token balance for challenge participation
- Be aware of challenge deadlines and conditions